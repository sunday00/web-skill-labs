local t = {
    x = 11.23,
    f = function ()
        print("hi")
    end,
}

t.f()

t.g = function()
    print("g")
end

t.g()

function t.h()
    print("h")
end

t.h()


print("-=-=-=-=-=-=-=-=-=-=-=-=-")

local tt = {
    x = 11.23,
    f = function(self, a)
        print(self.x)
    end
}

tt.f("hello")
tt:f("hello") -- method calling.

function tt:g (b)
    print(self.x .. b)
end

tt:g(" hello")
