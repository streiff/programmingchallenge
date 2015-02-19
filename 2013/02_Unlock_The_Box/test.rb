#!/usr/bin/env ruby

N = 4
str = $stdin.gets
0.upto(10**N - 1) do |n|
  s = sprintf("%0#{N}d", n)

  puts "Does not contain #{s}" unless str.include?(s)

end
