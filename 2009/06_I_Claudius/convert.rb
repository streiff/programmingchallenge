#!/usr/bin/ruby -w

NUMERALS = {1000 => 'M',
             900 => 'CM', 500 => 'D', 400 => 'CD', 100 => 'C',
              90 => 'XC',  50 => 'L',  40 => 'XL',  10 => 'X',
               9 => 'IX',   5 => 'V',   4 => 'IV',   1 => 'I'}

number = ARGV[0] =~ /^\d+$/ && (1..3999).member?(ARGV[0].to_i) ? ARGV[0].to_i : nil

if nil == number
  puts "The first command line argument (#{number}) must be a number between 1 and 3999 inclusive"
  exit 1
end

result = NUMERALS.keys.sort {|a, b| b <=> a}.inject ""  do |s, i|
   while number >= i do
     number -= i
     s += NUMERALS[i]
   end
   s
end

puts result

