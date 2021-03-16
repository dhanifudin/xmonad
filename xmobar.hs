Config  { font            = "xft:Kristi-16:bold"
        , bgColor         = "#333333"
        , fgColor         = "#FFFFFF"
        , position        = TopW L 100
        , lowerOnStart    = True
        , commands        = [ Run Date "%a %b %_d %l:%M" "date" 10
                            , Run StdinReader
                            ]
        , sepChar         = "%"
        , alignSep        = "}{"
        , template        = " XMonad | %StdinReader% }{ %date% "
        }
