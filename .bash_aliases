alias l='ls -CF'
alias ll='ls -lha'
alias la='ls -A'
alias grep='grep --color=auto'
alias ..='cd ..'
alias ...='cd ../../'

if [ `uname -s` = Darwin ]; then
    alias ls='ls -G'
    # Show/hide hidden files in Finder
    alias show="defaults write com.apple.Finder AppleShowAllFiles -bool true && killall Finder"
    alias hide="defaults write com.apple.Finder AppleShowAllFiles -bool false && killall Finder"
    alias amacs='open -b org.gnu.Aquamacs'

    script_path=`dirname $(python -c "import os; print os.path.realpath('${BASH_SOURCE[0]}')")`
    # echo `dirname $(perl -e 'use Cwd "abs_path";print abs_path(shift)' ${BASH_SOURCE[0]})`
else
    alias ls='ls --color=auto'

    script_path=`dirname $(readlink -f ${BASH_SOURCE[0]})`
fi
alias em='emacsclient -n -a "vi"'
source $script_path/.git-completion.sh
PS1='\u@\h: \[\033[01;32m\]\w\[\033[00m\] $(__git_ps1 "(\[\033[01;34m\]%s\[\033[00m\])")\$ '

# swap filename
sw() {
    if [ -e "$1~" ]; then
        mv "$1" "__$1__" && mv "$1~" "$1" && mv "__$1__" "$1~"
    else
        cp "$1" "$1~"
    fi
}

cmdfu(){
    curl "http://www.commandlinefu.com/commands/matching/$@/`echo -n $@ | base64`/plaintext";
}

# vim: ft=sh
