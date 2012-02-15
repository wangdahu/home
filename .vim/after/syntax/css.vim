" Language:	   Colored CSS Color Preview
" Maintainer:	Niklas Hofer <niklas+vim@lanpartei.de>
" URL:		   svn://lanpartei.de/vimrc/after/syntax/css.vim
" Last Change:	2008 Feb 12
" Licence:     No Warranties. Do whatever you want with this. But please tell me!
" Version:     0.6

function! s:GetBG(fg)
   " takes a rgb color code and returns a matching color that is visible
   let fg = a:fg
   return fg[0]*30 + fg[1]*59 + fg[2]*11 > 12000 ? 0 : 1
endfunction

function! s:HextoRGB(hex)
   let hex = a:hex
   return eval('[0x'.hex[0:1]. ',0x'.hex[2:3]. ',0x'.hex[4:5].']')
endfunction

function! s:RGBtoHex(rgb)
   let rgb = a:rgb
   return printf( '%02x%02x%02x', rgb[0], rgb[1], rgb[2])
endfunction

function! s:HSLtoHex(h,s,l)
  " Convert 80% -> 0.8, 100% -> 1.0, etc.
  let [s,l] = map( [a:s, a:l], 'v:val =~ "%$" ? v:val / 100.0 : v:val * 1.0' )
  " algorithm transcoded to vim from http://www.w3.org/TR/css3-color/#hsl-color
  let hh = ( a:h % 360 ) / 360.0
  let m2 = l <= 0.5 ? l * ( s + 1 ) : l + s - l * s
  let m1 = l * 2 - m2
  let rgb = []
  for h in [ hh + (1/3.0), hh, hh - (1/3.0) ]
    let h = h < 0 ? h + 1 : h > 1 ? h - 1 : h
    let v =
          \ h * 6 < 1 ? m1 + ( m2 - m1 ) * h * 6 :
          \ h * 2 < 1 ? m2 :
          \ h * 3 < 2 ? m1 + ( m2 - m1 ) * ( 2/3.0 - h ) * 6 :
          \ m1
    let rgb += [ float2nr( 255 * v ) ]
  endfor
  return s:RGBtoHex(rgb)
endfunction

function! s:SetGroup(group, clr)
   let rgb = s:HextoRGB(a:clr)
   exe 'syn cluster cssColors add='.a:group
   exe 'hi' a:group 'guibg=#'.a:clr 'guifg=#'.(s:GetBG(rgb) ? 'ffffff' : '000000')
endfunction

if &t_Co == 256

" SetGroup for xterm256
redir => s:source
silent! function s:SetGroup
redir END
let s:source = substitute( s:source, '\n\zs\d\+', '', 'g')
let s:source = substitute( s:source , 'function\zs', '!', '')
exe substitute( s:source, 'guibg.*)', "ctermbg='.s:Rgb2xterm(rgb) 'ctermfg='.s:Rgb2xterm(s:GetBG(rgb) ? [255, 255, 255] : [0, 0, 0])", '')

" the 6 value iterations in the xterm color cube
let s:valuerange = [ 0x00, 0x5F, 0x87, 0xAF, 0xD7, 0xFF ]

" 16 basic colors
let s:basic16 = [ [ 0x00, 0x00, 0x00 ], [ 0xCD, 0x00, 0x00 ], [ 0x00, 0xCD, 0x00 ], [ 0xCD, 0xCD, 0x00 ], [ 0x00, 0x00, 0xEE ], [ 0xCD, 0x00, 0xCD ], [ 0x00, 0xCD, 0xCD ], [ 0xE5, 0xE5, 0xE5 ], [ 0x7F, 0x7F, 0x7F ], [ 0xFF, 0x00, 0x00 ], [ 0x00, 0xFF, 0x00 ], [ 0xFF, 0xFF, 0x00 ], [ 0x5C, 0x5C, 0xFF ], [ 0xFF, 0x00, 0xFF ], [ 0x00, 0xFF, 0xFF ], [ 0xFF, 0xFF, 0xFF ] ]

function! s:Xterm2rgb(color)
   let rgb = [0, 0, 0]
   if a:color<16
      " 16 basic colors
      let rgb = s:basic16[a:color]
   elseif a:color<=232
      " color cube color
      let color=a:color-16
      let r = s:valuerange[(color/36)%6]
      let g = s:valuerange[(color/6)%6]
      let b = s:valuerange[color%6]
      let rgb = [r, g, b]
   elseif a:color<=253
      " gray tone
      let r = 8 + (a:color-232)*0x0a
      let rgb = [r, r, r]
   endif
   return rgb
endfunction

function! s:square(x)
   return a:x * a:x
endfunction

let s:colortable=[]
for c in range(0, 254)
   call add(s:colortable, s:Xterm2rgb(c))
endfor

" selects the nearest xterm color for a rgb value like [255, 0, 0]
function! s:Rgb2xterm(color)
   let best_match=0
   let smallest_distance = 10000000000
   let [r, g, b] = a:color
   for c in range(0,254)
      let d = s:square(s:colortable[c][0]-r) + s:square(s:colortable[c][1]-g) + s:square(s:colortable[c][2]-b)
      if d<smallest_distance
         let smallest_distance = d
         let best_match = c
         if d == 0 | break | endif
      endif
   endfor
   return best_match
endfunction
endif " &t_Co == 256

function! s:SetMatcher(clr,pat)
   let group = 'cssColor'.a:clr
   let pattern = a:pat
   if pattern =~ '\>$' | let pattern .= '\>' | endif
   redir => currentmatch
      silent! exe 'syn list' group
   redir END
   if stridx( currentmatch, 'match /'.pattern.'/' ) >= 0 | return 0 | endif
   exe 'syn match' group '/'.pattern.'/ contained'
   call s:SetGroup(group, a:clr)
   return 1
endfunction

function! s:SetNamedColor(clr, name)
   let group = 'cssColor'. a:clr
   exe 'syn keyword' group a:name 'contained'
   call s:SetGroup(group, a:clr)
endfunction

function! s:PreviewCSSColor(text)
   let text = substitute(a:text, '#\(\x\)\(\x\)\(\x\)\>', '\=s:SetMatcher(submatch(1).submatch(1).submatch(2).submatch(2).submatch(3).submatch(3), submatch(0))', 'g')
   let text = substitute(text, '#\(\x\{6\}\)\>', '\=s:SetMatcher(submatch(1), submatch(0))', 'g')
   let text = substitute(text, 'rgba\?(\(\d\{1,3}\),\s*\(\d\{1,3}\),\s*\(\d\{1,3}\)\%(,[^)]*\)\?)', '\=s:SetMatcher(s:RGBtoHex([submatch(1), submatch(2), submatch(3)]), submatch(0))', 'g')
   let text = substitute(text, 'hsla\?(\(\d\{1,3}\),\s*\(\d\{1,3}%\),\s*\(\d\{1,3}%\)\%(,[^)]*\)\?)', '\=s:SetMatcher(s:HSLtoHex(submatch(1), submatch(2), submatch(3)), submatch(0))', 'g')
endfunction

if has("gui_running") || &t_Co==256
   " HACK modify cssDefinition to add @cssColors to its contains
   redir => s:olddef
      silent!  syn list cssDefinition
   redir END
   if s:olddef != ''
      let s:b = strridx(s:olddef,'matchgroup')
      if s:b != -1
         exe 'syn region cssDefinition '.strpart(s:olddef,s:b).',@cssColors'
      endif
   endif

   " w3c Colors
   call s:SetNamedColor('800000', 'maroon')
   call s:SetNamedColor('ff0000', 'red')
   call s:SetNamedColor('ffA500', 'orange')
   call s:SetNamedColor('ffff00', 'yellow')
   call s:SetNamedColor('808000', 'olive')
   call s:SetNamedColor('800080', 'purple')
   call s:SetNamedColor('ff00ff', 'fuchsia')
   call s:SetNamedColor('ffffff', 'white')
   call s:SetNamedColor('00ff00', 'lime')
   call s:SetNamedColor('008000', 'green')
   call s:SetNamedColor('000080', 'navy')
   call s:SetNamedColor('0000ff', 'blue')
   call s:SetNamedColor('00ffff', 'aqua')
   call s:SetNamedColor('008080', 'teal')
   call s:SetNamedColor('000000', 'black')
   call s:SetNamedColor('c0c0c0', 'silver')
   call s:SetNamedColor('808080', 'gray')

   " extra colors
   call s:SetNamedColor('F0F8FF','AliceBlue')
   call s:SetNamedColor('FAEBD7','AntiqueWhite')
   call s:SetNamedColor('7FFFD4','Aquamarine')
   call s:SetNamedColor('F0FFFF','Azure')
   call s:SetNamedColor('F5F5DC','Beige')
   call s:SetNamedColor('FFE4C4','Bisque')
   call s:SetNamedColor('FFEBCD','BlanchedAlmond')
   call s:SetNamedColor('8A2BE2','BlueViolet')
   call s:SetNamedColor('A52A2A','Brown')
   call s:SetNamedColor('DEB887','BurlyWood')
   call s:SetNamedColor('5F9EA0','CadetBlue')
   call s:SetNamedColor('7FFF00','Chartreuse')
   call s:SetNamedColor('D2691E','Chocolate')
   call s:SetNamedColor('FF7F50','Coral')
   call s:SetNamedColor('6495ED','CornflowerBlue')
   call s:SetNamedColor('FFF8DC','Cornsilk')
   call s:SetNamedColor('DC143C','Crimson')
   call s:SetNamedColor('00FFFF','Cyan')
   call s:SetNamedColor('00008B','DarkBlue')
   call s:SetNamedColor('008B8B','DarkCyan')
   call s:SetNamedColor('B8860B','DarkGoldenRod')
   call s:SetNamedColor('A9A9A9','DarkGray')
   call s:SetNamedColor('A9A9A9','DarkGrey')
   call s:SetNamedColor('006400','DarkGreen')
   call s:SetNamedColor('BDB76B','DarkKhaki')
   call s:SetNamedColor('8B008B','DarkMagenta')
   call s:SetNamedColor('556B2F','DarkOliveGreen')
   call s:SetNamedColor('FF8C00','Darkorange')
   call s:SetNamedColor('9932CC','DarkOrchid')
   call s:SetNamedColor('8B0000','DarkRed')
   call s:SetNamedColor('E9967A','DarkSalmon')
   call s:SetNamedColor('8FBC8F','DarkSeaGreen')
   call s:SetNamedColor('483D8B','DarkSlateBlue')
   call s:SetNamedColor('2F4F4F','DarkSlateGray')
   call s:SetNamedColor('2F4F4F','DarkSlateGrey')
   call s:SetNamedColor('00CED1','DarkTurquoise')
   call s:SetNamedColor('9400D3','DarkViolet')
   call s:SetNamedColor('FF1493','DeepPink')
   call s:SetNamedColor('00BFFF','DeepSkyBlue')
   call s:SetNamedColor('696969','DimGray')
   call s:SetNamedColor('696969','DimGrey')
   call s:SetNamedColor('1E90FF','DodgerBlue')
   call s:SetNamedColor('B22222','FireBrick')
   call s:SetNamedColor('FFFAF0','FloralWhite')
   call s:SetNamedColor('228B22','ForestGreen')
   call s:SetNamedColor('DCDCDC','Gainsboro')
   call s:SetNamedColor('F8F8FF','GhostWhite')
   call s:SetNamedColor('FFD700','Gold')
   call s:SetNamedColor('DAA520','GoldenRod')
   call s:SetNamedColor('808080','Grey')
   call s:SetNamedColor('ADFF2F','GreenYellow')
   call s:SetNamedColor('F0FFF0','HoneyDew')
   call s:SetNamedColor('FF69B4','HotPink')
   call s:SetNamedColor('CD5C5C','IndianRed')
   call s:SetNamedColor('4B0082','Indigo')
   call s:SetNamedColor('FFFFF0','Ivory')
   call s:SetNamedColor('F0E68C','Khaki')
   call s:SetNamedColor('E6E6FA','Lavender')
   call s:SetNamedColor('FFF0F5','LavenderBlush')
   call s:SetNamedColor('7CFC00','LawnGreen')
   call s:SetNamedColor('FFFACD','LemonChiffon')
   call s:SetNamedColor('ADD8E6','LightBlue')
   call s:SetNamedColor('F08080','LightCoral')
   call s:SetNamedColor('E0FFFF','LightCyan')
   call s:SetNamedColor('FAFAD2','LightGoldenRodYellow')
   call s:SetNamedColor('D3D3D3','LightGray')
   call s:SetNamedColor('D3D3D3','LightGrey')
   call s:SetNamedColor('90EE90','LightGreen')
   call s:SetNamedColor('FFB6C1','LightPink')
   call s:SetNamedColor('FFA07A','LightSalmon')
   call s:SetNamedColor('20B2AA','LightSeaGreen')
   call s:SetNamedColor('87CEFA','LightSkyBlue')
   call s:SetNamedColor('778899','LightSlateGray')
   call s:SetNamedColor('778899','LightSlateGrey')
   call s:SetNamedColor('B0C4DE','LightSteelBlue')
   call s:SetNamedColor('FFFFE0','LightYellow')
   call s:SetNamedColor('32CD32','LimeGreen')
   call s:SetNamedColor('FAF0E6','Linen')
   call s:SetNamedColor('FF00FF','Magenta')
   call s:SetNamedColor('66CDAA','MediumAquaMarine')
   call s:SetNamedColor('0000CD','MediumBlue')
   call s:SetNamedColor('BA55D3','MediumOrchid')
   call s:SetNamedColor('9370D8','MediumPurple')
   call s:SetNamedColor('3CB371','MediumSeaGreen')
   call s:SetNamedColor('7B68EE','MediumSlateBlue')
   call s:SetNamedColor('00FA9A','MediumSpringGreen')
   call s:SetNamedColor('48D1CC','MediumTurquoise')
   call s:SetNamedColor('C71585','MediumVioletRed')
   call s:SetNamedColor('191970','MidnightBlue')
   call s:SetNamedColor('F5FFFA','MintCream')
   call s:SetNamedColor('FFE4E1','MistyRose')
   call s:SetNamedColor('FFE4B5','Moccasin')
   call s:SetNamedColor('FFDEAD','NavajoWhite')
   call s:SetNamedColor('FDF5E6','OldLace')
   call s:SetNamedColor('6B8E23','OliveDrab')
   call s:SetNamedColor('FF4500','OrangeRed')
   call s:SetNamedColor('DA70D6','Orchid')
   call s:SetNamedColor('EEE8AA','PaleGoldenRod')
   call s:SetNamedColor('98FB98','PaleGreen')
   call s:SetNamedColor('AFEEEE','PaleTurquoise')
   call s:SetNamedColor('D87093','PaleVioletRed')
   call s:SetNamedColor('FFEFD5','PapayaWhip')
   call s:SetNamedColor('FFDAB9','PeachPuff')
   call s:SetNamedColor('CD853F','Peru')
   call s:SetNamedColor('FFC0CB','Pink')
   call s:SetNamedColor('DDA0DD','Plum')
   call s:SetNamedColor('B0E0E6','PowderBlue')
   call s:SetNamedColor('BC8F8F','RosyBrown')
   call s:SetNamedColor('4169E1','RoyalBlue')
   call s:SetNamedColor('8B4513','SaddleBrown')
   call s:SetNamedColor('FA8072','Salmon')
   call s:SetNamedColor('F4A460','SandyBrown')
   call s:SetNamedColor('2E8B57','SeaGreen')
   call s:SetNamedColor('FFF5EE','SeaShell')
   call s:SetNamedColor('A0522D','Sienna')
   call s:SetNamedColor('87CEEB','SkyBlue')
   call s:SetNamedColor('6A5ACD','SlateBlue')
   call s:SetNamedColor('708090','SlateGray')
   call s:SetNamedColor('708090','SlateGrey')
   call s:SetNamedColor('FFFAFA','Snow')
   call s:SetNamedColor('00FF7F','SpringGreen')
   call s:SetNamedColor('4682B4','SteelBlue')
   call s:SetNamedColor('D2B48C','Tan')
   call s:SetNamedColor('D8BFD8','Thistle')
   call s:SetNamedColor('FF6347','Tomato')
   call s:SetNamedColor('40E0D0','Turquoise')
   call s:SetNamedColor('EE82EE','Violet')
   call s:SetNamedColor('F5DEB3','Wheat')
   call s:SetNamedColor('F5F5F5','WhiteSmoke')
   call s:SetNamedColor('9ACD32','YellowGreen')


   call s:PreviewCSSColor(join(getline(1, '$')))

   autocmd CursorHold * silent call s:PreviewCSSColor(getline('.'))
   autocmd CursorHoldI * silent call s:PreviewCSSColor(getline('.'))
   set ut=500
endif		" has("gui_running")

" vim: sts=3:sw=3
