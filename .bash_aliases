alias l='ls -CF'
alias ll='ls -lha'
alias la='ls -A'
alias grep='grep --color=auto'
alias ..='cd ..'
alias ...='cd ../../'
alias py=python

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
        package="com.apple.Finder"
        case ${1:-allfile} in
            allfile) name="AppleShowAllFiles";;
            desktop) name="CreateDesktop";;
            ext) name="AppleShowAllExtensions";package="NSGlobalDomain";;
        esac
        if [ "$2" = "" ]; then
            state=`defaults read $package $name`
            if [ "$state" = 0 ]; then val=1; fi
        else
            val="$2";
        fi
        if [ "$val" = 1 ]; then val=true; else val=false; fi
        defaults write $package $name -bool $val && killall Finder
    }

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

cmdfu() {
    curl "http://www.commandlinefu.com/commands/matching/$@/`echo -n $@ | base64`/plaintext";
}

# vim: ft=sh
