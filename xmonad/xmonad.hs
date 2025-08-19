import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.IndependentScreens
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.NamedScratchpad
import XMonad.Hooks.ManageHelpers
import XMonad.Actions.SpawnOn
import qualified XMonad.StackSet as W
import System.IO

-- Scratchpad definition
scratchpads = [
    NS "scratchpad" "alacritty --class xmonad-scratchpad,xmonad-scratchpad" 
       (className =? "xmonad-scratchpad") 
       (customFloating $ W.RationalRect 0.15 0.0 0.7 0.35)
    ]

-- Spawn terminal on current screen
spawnTerminalHere :: X ()
spawnTerminalHere = spawnHere "alacritty"

main = do
    xmproc <- spawnPipe "xmobar"
    spawn "xmodmap ~/.Xmodmap 2>/dev/null || true"
    -- Start EWW daemon and widgets with proper display detection
    spawn "eww kill 2>/dev/null; eww daemon"
    spawn "sleep 3 && ~/.config/xmonad/scripts/start-eww-widgets.sh"
    
    xmonad $ ewmhFullscreen $ ewmh $ docks def
        { manageHook = manageDocks 
                      <+> namedScratchpadManageHook scratchpads
                      <+> (className =? "xmonad-scratchpad" --> doFloat)
                      <+> (className =? "InputLeap" --> doFloat)
                      <+> (className =? "input-leap" --> doFloat)
                      <+> (className =? "Input-leap" --> doFloat)
                      <+> (className =? "input-leap-terminal" --> doFloat)
                      <+> (title =? "Input Leap" --> doFloat)
                      <+> (title =? "Configure Server" --> doFloat)
                      <+> manageSpawn
                      <+> manageHook def
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
        [ 
        -- Terminal spawn on current screen
          ((mod4Mask .|. shiftMask, xK_Return), spawnTerminalHere)
        
        -- Existing keybindings
        , ((mod4Mask .|. shiftMask, xK_z), spawn "~/.config/xmonad/lockscreen/lock.sh")
        , ((0, xK_Print), spawn "flameshot gui")
        , ((shiftMask, xK_Print), spawn "flameshot full -c")
        , ((controlMask, xK_Print), spawn "flameshot screen -c")
        , ((mod4Mask, xK_p), spawn "PATH=$HOME/bin:$PATH dmenu_run")
        , ((mod4Mask, xK_n), spawn "networkmanager_dmenu")
        , ((controlMask .|. shiftMask, xK_c), spawn "xclip -selection clipboard")
        , ((controlMask .|. shiftMask, xK_v), spawn "xclip -selection clipboard -o | xdotool type --clearmodifiers --file -")
        
        -- New feature keybindings
        -- Scratchpad terminal (Super + `)
        , ((mod4Mask, xK_grave), namedScratchpadAction scratchpads "scratchpad")
        
        -- Theme selector (Super + Shift + t)
        , ((mod4Mask .|. shiftMask, xK_t), spawn "~/.config/xmonad/theme-switcher/theme-selector.sh")
        
        -- Toggle EWW widgets (Super + e)
        , ((mod4Mask, xK_e), spawn "~/.config/xmonad/scripts/toggle-eww.sh")
        
        -- Show cheatsheet (Super + F1)
        , ((mod4Mask, xK_F1), spawn "~/.config/xmonad/scripts/toggle-cheatsheet.sh")
        
        -- Power menu (Super + Shift + p)
        , ((mod4Mask .|. shiftMask, xK_p), spawn "~/.config/xmonad/scripts/toggle-power-menu.sh")
        
        -- Lock screen with wallpaper (Super + l)
        , ((mod4Mask, xK_l), spawn "~/.config/xmonad/lockscreen/lock.sh -w")
        
        -- Media keys
        , ((0, 0x1008FF11), spawn "amixer -D pulse sset Master 5%-")  -- XF86AudioLowerVolume
        , ((0, 0x1008FF13), spawn "amixer -D pulse sset Master 5%+")  -- XF86AudioRaiseVolume
        , ((0, 0x1008FF12), spawn "amixer -D pulse sset Master toggle") -- XF86AudioMute
        
        -- Brightness keys
        , ((0, 0x1008FF03), spawn "brightnessctl s 5%-") -- XF86MonBrightnessDown
        , ((0, 0x1008FF02), spawn "brightnessctl s 5%+") -- XF86MonBrightnessUp
        
        -- Input Leap (Super + Shift + i)
        , ((mod4Mask .|. shiftMask, xK_i), spawn "~/.config/xmonad/scripts/input-leap-wrapper.sh")
        ]