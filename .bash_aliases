alias l='ls -CF'
alias ll='ls -lha'
alias la='ls -A'
alias grep='grep --color=auto'
alias ..='cd ..'
alias ...='cd ../../'
alias py=python
alias so=source
complete -W "$(sed -E 's/[, ].*//' ~/.ssh/known_hosts)" ssh

if [ `uname -s` = Darwin ]; then
    alias ls='ls -G'
    alias aq='open -b org.gnu.Aquamacs'

    # show or hide All Files/Desktop Icons/Filename extentions in Finder
    # Usage:
    #       show <command> [switch]
    #       command     can be allfile/desktop/ext
    # $ show allfile
    # or
    # $ show desktop 1
    show() {
        local package="com.apple.Finder"
        local name="AppleShowAllFiles"
        case ${1:-allfile} in
            desktop) name="CreateDesktop";;
            ext) name="AppleShowAllExtensions";package="NSGlobalDomain";;
        esac
        local val="${2:-1}"
        if [ -z "$2" ]; then
            [ `defaults read $package $name` = 1 ] && val=0
        fi
        if [ "$val" = 1 ]; then val=true; else val=false; fi
        defaults write $package $name -bool $val && killall Finder
    }
    complete -W "allfile desktop ext" show

    script_path=`dirname $(python -c "import os; print os.path.realpath('${BASH_SOURCE[0]}')")`
    # echo `dirname $(perl -e 'use Cwd "abs_path";print abs_path(shift)' ${BASH_SOURCE[0]})`
else
    alias ls='ls --color=auto'

    script_path=`dirname $(readlink -f ${BASH_SOURCE[0]})`
fi
alias em='emacsclient -na "vi"'
source $script_path/.git-completion.sh
PS1='\u@\h: \[\033[01;32m\]\w\[\033[00m\] $(__git_ps1 "(\[\033[01;34m\]%s\[\033[00m\])")\$ '

# swap filename
sw() {
    if [ -e "$1~" ]; then
        mv "$1" "__$1__" && mv "$1"{~,} && mv "__$1__" "$1~"
    else
        cp "$1"{,~}
    fi
}

# Jump up to any directory above the current one
upto() { cd "${PWD/\/$@\/*//$@}"; }
_complete_upto() {
    local IFS=$'\n'
    local word=${COMP_WORDS[COMP_CWORD]}
    COMPREPLY=($(echo ${PWD#/} | sed 's|/|\n|g' | grep -i "^$word" | sed -e 's| |\\ |g'))
}
complete -F _complete_upto upto

# Create a data URI from an image
datauri() {
    local file=$1
    if [[ "$1" =~ ^https?:// ]]; then
        file=/tmp/${1##*/}
        curl -o "$file" "$1"
    fi
    [ -f "$file" ] && echo "data:image/${1##*.};base64,$(openssl base64 -in "$file")" | tr -d '\n'
}

cmdfu() {
    curl "http://www.commandlinefu.com/commands/matching/$@/`echo -n $@ | openssl base64`/plaintext";
}

st() {
    local editor=${2:-echo};
    if [ -z "$1" ]; then
        git status -s | nl -w3
    else
        $editor $3 "$(git status -s | nl -w3 | head -n$1 | tail -n1 | cut -c8-)"
    fi
}

# vim: ft=sh
