local settings = require "settings"
local modes = require "modes"

local engines = settings.window.search_engines

engines.ecosia =
    "https://www.ecosia.org/search?method=index&q=%s"

engines.default = engines.ecosia

settings.window.home_page = "luakit://newtab"
settings.window.close_with_last_tab = true

local modes = require "modes"

modes.add_binds("normal", {
    {"<Control-w>", "Chiudi scheda", function (w)
        w:close_tab()
    end},

    {"<Control-t>", "Nuova scheda", function (w)
        w:enter_cmd(":tabopen ")
    end},

    {"<Control-l>", "Apri URL o cerca", function (w)
        w:enter_cmd(":open ")
    end},

    {"<Alt-Left>", "Indietro", function (w)
        w:back()
    end},

    {"<Alt-Right>", "Avanti", function (w)
        w:forward()
    end},

    {"<Control-r>", "Ricarica pagina", function (w)
        w:reload()
    end},

    { "<Control-c>", "Copy selected text.", function ()
        luakit.selection.clipboard = luakit.selection.primary
    end},

    {"<Control-s>", "Cerca nella pagina", function (w)
        w:enter_cmd("/")
    end},
})