alias l='ls -CF'
alias ll='ls -lha'
alias la='ls -A'
alias grep='grep --color=auto'
alias ..='cd ..'
alias ...='cd ../../'
alias py=python
alias so=source
alias wangsir='cd /var/www/wangsir'
alias c='cd /var/www/c'
alias st='git st'
alias ms='git co master'
alias pms='git push origin master'
alias pl='git pull'
alias grp='la | grep'
alias wg='cd /var/www'
alias si='sudo vi'
alias rr='rm -r'
alias vpn='python /home/wg/Document/local.py -s uedsky.com -p 7557 -l 3838 -k ZjE5MDU3M2Ex'
alias dpubl='cd ~/Dropbox/Public/'

md(){
	mkdir -p "$@" && cd $_;
}
