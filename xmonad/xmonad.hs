import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.IndependentScreens
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main = do
    xmproc <- spawnPipe "xmobar"
    spawn "xmodmap ~/.Xmodmap"
    xmonad $ ewmhFullscreen $ ewmh $ docks def
        { manageHook = manageDocks <+> manageHook def
        , layoutHook = avoidStruts  $  layoutHook def
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Super key
        , terminal = "alacritty"
        , borderWidth = 2
        , normalBorderColor = "#cccccc"
        , focusedBorderColor = "#cd8b00"
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        , ((mod4Mask, xK_p), spawn "dmenu_run")
        -- Clipboard keybindings (similar to Ctrl+Shift+V/C)
        , ((controlMask .|. shiftMask, xK_c), spawn "xclip -selection clipboard")
        , ((controlMask .|. shiftMask, xK_v), spawn "xclip -selection clipboard -o | xdotool type --clearmodifiers --file -")
        ]