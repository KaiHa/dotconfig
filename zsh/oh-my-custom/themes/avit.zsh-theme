. ~/.oh-my-zsh/themes/avit.zsh-theme

local ret_status1="%(?:%{$fg[green]%}╭─ %{$reset_color%}:%{$fg[red]%}╭─ %{$reset_color%}%s)"
local ret_status2="%(?:%{$fg[green]%}╰➜ %{$reset_color%}:%{$fg[red]%}╰➜ %{$reset_color%}%s)"

PROMPT='
${ret_status1}$(_user_host)${_current_dir} $(git_prompt_info)
${ret_status2}'

RPROMPT='%{$(echotc UP 1)%} ⌚ $(date +%H:%M:%S) $(git_prompt_status) ${_return_status}%{$(echotc DO 1)%}'

export LSCOLORS=""
export LS_COLORS=''
export GREP_COLOR=''
