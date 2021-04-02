function love.conf(t)
    t.identity = nil
    t.version = "11.3"
    t.console = false
    t.window.title = ""
    t.window.icon = nil
    t.window.width = 900
    t.window.height = 600
    t.window.borderless = false
    t.window.resizable = true
    t.window.minwidth = 600
    t.window.minheight = 400
    t.window.fullscreen = false
    t.window.vsync = true
    t.window.fsaa = 0
    t.window.display = 1
    t.window.highdpi = false
    t.window.srgb = false
    t.window.x = nil
    t.window.y = nil
    t.modules.audio = true
    t.modules.event = true
    t.modules.graphics = true
    t.modules.image = true
    t.modules.joystick = false
    t.modules.keyboard = true
    t.modules.math = true
    t.modules.mouse = true
    t.modules.physics = false
    t.modules.sound = true
    t.modules.system = true
    t.modules.timer = true
    t.modules.window = true
end
