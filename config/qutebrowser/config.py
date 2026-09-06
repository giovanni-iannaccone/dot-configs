config.load_autoconfig()

c.input.insert_mode.auto_enter = False

c.input.forward_unbound_keys = "all"

c.url.searchengines = {"DEFAULT": "https://www.ecosia.org/search?method=index&q={}"}

config.bind("<Ctrl-a>", "rl-beginning-of-line", mode="insert")
config.bind("<Ctrl-e>", "rl-end-of-line", mode="insert")

config.bind("<Ctrl-b>", "rl-backward-char", mode="insert")
config.bind("<Ctrl-f>", "rl-forward-char", mode="insert")

config.bind("<Alt-b>", "rl-backward-word", mode="insert")
config.bind("<Alt-f>", "rl-forward-word", mode="insert")

config.bind("<Ctrl-d>", "rl-delete-char", mode="insert")
config.bind("<Ctrl-h>", "rl-backward-delete-char", mode="insert")

config.bind("<Ctrl-k>", "rl-kill-line", mode="insert")
config.bind("<Ctrl-u>", "rl-unix-line-discard", mode="insert")
config.bind("<Ctrl-w>", "rl-unix-word-rubout", mode="insert")

config.bind("<Ctrl-y>", "rl-yank", mode="insert")

config.bind("<Ctrl-n>", "scroll down")
config.bind("<Ctrl-p>", "scroll up")

config.bind("<Alt-v>", "scroll-page 0 -1")
config.bind("<Ctrl-v>", "scroll-page 0 1")

config.bind("<Ctrl-l>", "cmd-set-text :open ")

config.bind("<Ctrl-s>", "cmd-set-text /")

config.bind("<Ctrl-r>", "search-prev")
config.bind("<Ctrl-s>", "search-next", mode="normal")

config.bind("<Ctrl-w>", "tab-close")

config.bind("<Ctrl-t>", "open -t")

config.bind("<Alt-1>", "tab-focus 1")
config.bind("<Alt-2>", "tab-focus 2")
config.bind("<Alt-3>", "tab-focus 3")
config.bind("<Alt-4>", "tab-focus 4")
config.bind("<Alt-5>", "tab-focus 5")
config.bind("<Alt-6>", "tab-focus 6")
config.bind("<Alt-7>", "tab-focus 7")
config.bind("<Alt-8>", "tab-focus 8")
config.bind("<Alt-9>", "tab-focus -1")

config.bind("<Alt-left>", "back")
config.bind("<Alt-right>", "forward")

config.bind("<Ctrl-r>", "reload", mode="normal")

config.bind("<Ctrl-=>", "zoom-in")
config.bind("<Ctrl-->", "zoom-out")
config.bind("<Ctrl-0>", "zoom")

config.bind("<Ctrl-y>", "yank", mode="normal")
