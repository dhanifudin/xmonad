import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Tabbed
import XMonad.Util.Cursor
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Replace
import XMonad.Util.Run(spawnPipe)
import XMonad.Prompt
import System.Exit 
import System.IO
import qualified XMonad.StackSet as W
import qualified Data.Map as M

myFont                      = "xft:Fira Code-10:bold"
ppTitleFgColor              = "#FFFFFF"
ppTitleBgColor              = "#333333"
ppCurrentFgColor            = "#1793D1"
ppCurrentBgColor            = "#333333"
ppUrgentFgColor             = "#FF0000"
ppUrgentBgColor             = "#333333"
ppHiddenFgColor             = "#999999"
ppHiddenBgColor             = "#333333"
ppSepFgColor                = ""
ppSepBgColor                = ""
ppWsSepFgColor              = "#1793D1"
ppWsSepBgColor              = ""

ppSeparator                 = ""
ppWsSeparator               = ""

myNormalBorderColor         = "#333333"
myFocusedBorderColor        = "#0088CC"

myBorderWidth               = 4
myModMask                   = mod4Mask
myTerminal                  = "terminator"
myWorkspaces                = ["Q","W","E","R","T","Y","U","I","O", "P"]

myManageHook                = composeAll
    [ className             =? "Telegram"       --> doShift "P"
    , className             =? "firefox"        --> doShift "W"
    ]

myLayoutHook                = onWorkspace "Q" termLayout $ onWorkspace "W" webLayout $ onWorkspace "R" tabbedLayout $ defaultLayout
    where
        defaultLayout       = avoidStruts ( tall ||| Mirror tall ||| Full )
            where 
                tall        = Tall nmaster delta ratio
                nmaster     = 1
                ratio       = 2/3
                delta       = 2/100

        termLayout          = avoidStruts ( Grid ||| Full )

        webLayout           = avoidStruts ( Mirror tall ||| tall ||| Full)
            where 
                tall        = Tall nmaster delta ratio
                nmaster     = 1
                ratio       = 3/4
                delta       = 2/100 

        tabbedLayout        = avoidStruts ( tabbed shrinkText tabConfig ||| Full) 

tabConfig = defaultTheme 
    { fontName              = myFont
    , activeBorderColor     = "#1793D1"
    , activeTextColor       = "#333333"
    , activeColor           = "#1793D1"
    , inactiveBorderColor   = "#333333"
    , inactiveTextColor     = "#FFFFFF"
    , inactiveColor         = "#333333"
    }

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((0, 0x1008ff12), spawn "amixer set Master toggle")
    , ((0, 0x1008ff11), spawn "amixer set Master 5%-")
    , ((0, 0x1008ff13), spawn "amixer set Master 5%+")
    , ((modMask, xK_space), spawn "rofi -show run")
    , ((modMask, xK_s), spawn "scrot --delay 1 ~/Pictures/screenshots/screen_%Y-%m-%d %H-%M-%S.png")
    , ((modMask .|. shiftMask, xK_s), spawn "scrot --select ~/Pictures/screenshots/window_%Y-%m-%d %H-%M-%S.png")
    , ((modMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask, xK_x), kill)
    , ((modMask, xK_Tab), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_Tab), setLayout $XMonad.layoutHook conf)
    , ((modMask, xK_j), windows W.focusDown)
    , ((modMask, xK_k), windows W.focusUp)
    , ((modMask, xK_m), windows W.focusMaster)
    , ((modMask .|. shiftMask, xK_m), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j), windows W.swapDown)
    , ((modMask .|. shiftMask, xK_k), windows W.swapUp)
    , ((modMask, xK_h), sendMessage Shrink)
    , ((modMask, xK_l), sendMessage Expand)
    -- , ((modMask, xK_t), withFocused $ windows . W.sink)
    , ((modMask, xK_comma), sendMessage (IncMasterN 1))
    , ((modMask, xK_period), sendMessage (IncMasterN (-1)))
    , ((modMask .|. shiftMask, xK_x), io (exitWith ExitSuccess))
    --, ((modMask .|. shiftMask, xK_r), restart "xmonad" True)
    ]
    ++

    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_q, xK_w, xK_e, xK_r, xK_t, xK_y, xK_u, xK_i, xK_o, xK_p]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    ]

myStartupHook :: X ()
myStartupHook = do 
    setDefaultCursor xC_left_ptr >> setWMName "LG3D"
    spawn "~/.xmonad/autostart"

main = do
    xmonad $ docks $ ewmh def 
        { terminal              = myTerminal
        , borderWidth           = myBorderWidth
        , focusFollowsMouse     = myFocusFollowsMouse
        , modMask               = myModMask
        , workspaces            = myWorkspaces
        , normalBorderColor     = myNormalBorderColor
        , focusedBorderColor    = myFocusedBorderColor
        , keys                  = myKeys
        , mouseBindings         = myMouseBindings
        , logHook               = ewmhDesktopsLogHook
        , layoutHook            = myLayoutHook
        , manageHook            = manageDocks <+> myManageHook
        , startupHook           = myStartupHook
        }
