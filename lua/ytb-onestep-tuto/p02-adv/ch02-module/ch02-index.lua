--require("./ch02-some-01")
require "ch02-some-01"

print("main")

local m = require("ch02-some-02")

print(m(1, 2))

local mm = require("ch02-some-03")

print(mm.mul(2, 3), mm.sub(5, 3))

local fm = require('libs.ch02-lib')
fm(7, 7)
