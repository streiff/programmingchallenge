#!/usr/bin/env ruby

require 'optparse'
require 'set'

n = 4
optparse = OptionParser.new do |opts|
  opts.on("-h", "--help", "Display this screen") do
    puts opts
    exit
  end

  opts.on("-l length", "--length length", "Size of the code (default 4)") do |n|
    begin
      n = Integer(n)
    rescue
      puts "Length must be an integer"
      exit
    end
  end
end
optparse.parse!

codes = Set.new
0.upto(10**n - 1) {|i| codes.add(sprintf("%0#{n}d", i))}

str = "0" * n
codes.delete(str)
 
until codes.empty?
  last = str[-(n-1)..str.length]
  found = false
  0.upto(9) do |i|
    if (codes.include?("#{last}#{i}"))
      str += i.to_s
      found = true
      break
    end
  end
 
  str += codes.to_a[0] unless found
 
  codes.delete(str[-(n)..str.length])
end

puts str
