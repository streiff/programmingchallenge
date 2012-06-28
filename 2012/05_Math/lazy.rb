#!/usr/bin/env ruby

require 'open-uri'

i = gets.chomp.gsub(/multiply/, "multiplied by").gsub(/divide/, "divided by").gsub(/ /, "+")
URI.parse("http://www.google.com/search?num=1&q=#{i}").read =~ /<h2 class="r" dir="ltr" style="font-size:138%">(.*?)<\/h2>/
puts $1.index(".") == nil ? $1.split("=")[1].strip : "I have a bad feeling about this."
