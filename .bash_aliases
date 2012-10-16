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
PS1="\e[32;40m\u@\h: \w$\e[0m "

md(){
	mkdir -p "$@" && cd $_;
}
