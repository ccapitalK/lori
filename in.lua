require('utility')
grid = require('grid')
players = require('player')
arena = require('arena')
mainMenu = require('mainMenu')
options = require('options')
optionsMenu = require('optionsMenu')
state = require('state')
message = require('message')

function love.load()
    love.window.setTitle("NORT")
    love.window.setMode(600, 600, {})
    state.load()
    arena.load()
    mainMenu.load()
    options.load()
    optionsMenu.load()
end

function love.keypressed(key, scancode, isrepeat)
    if message.isShowingMSG() then
        message.parseInputs(key, scancode, isrepeat)
    else
        if state.state=='arena' then
            arena.parseInputs(key, scancode, isrepeat)
        end
    end
end

function love.mousepressed(x, y, button, istouch)
    if message.isShowingMSG() then
        message.mousepressed(x, y, button, istouch)
    else
        if state.state=='mainMenu' then
            mainMenu.mousepressed(x, y, button, istouch)
        elseif state.state=='optionsMenu' then
            optionsMenu.mousepressed(x, y, button, istouch)
        end
    end
end

function love.update(delta)
    if message.isShowingMSG() then
        message.update(delta)
    else
        if state.state=='arena' then
            arena.update(delta)
        elseif state.state=='mainMenu' then
            mainMenu.update()
        elseif state.state=='optionsMenu' then
            optionsMenu.update()
        end
    end
end

function love.draw()
    love.graphics.clear(colors.background) --[[
        Test multiline comment
    ]]--
    if state.state=='arena' then
        arena.draw()
    elseif state.state=='mainMenu' then
        mainMenu.render()
    elseif state.state=='optionsMenu' then
        optionsMenu.render()
    end
    if message.isShowingMSG() then
        message.render()
    end -- Test line comment
end

foo = [[ 
    Test multiline 1
]]

foo = [=[ 
    ]] ]==] Test multiline 2
]=]

foo = [=[ 
    Test multiline 3 -- & comment
]=]

