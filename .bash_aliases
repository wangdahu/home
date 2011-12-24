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

    script_path=`dirname $(python -c "import os; print os.path.realpath('${BASH_SOURCE[0]}')")`
    # echo `dirname $(perl -e 'use Cwd "abs_path";print abs_path(shift)' ${BASH_SOURCE[0]})`
else
    alias ls='ls --color=auto'
    alias ems='emacsclient -n -a vi'

    script_path=`dirname $(readlink -f ${BASH_SOURCE[0]})`
fi
source $script_path/.git-completion.sh
PS1='\u@\h: \[\033[01;32m\]\w\[\033[00m\] (\[\033[01;34m\]$(__git_ps1 "%s")\[\033[00m\])\$ '

cmdfu(){
    curl "http://www.commandlinefu.com/commands/matching/$@/`echo -n $@ | base64`/plaintext";
}

# vim: ft=sh
