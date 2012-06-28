#!/usr/bin/env ruby

def create_number(i)
  if (i == 0)
    "zero"
  elsif (i == 1)
    "one"
  elsif (i == 2)
    "two"
  elsif (i == 3)
    "three"
  elsif (i == 4)
    "four"
  elsif (i == 5)
    "five"
  elsif (i == 6)
    "six"
  elsif (i == 7)
    "seven"
  elsif (i == 8)
    "eight"
  elsif (i == 9)
    "nine"
  elsif (i == 10)
    "ten"
  elsif (i == 11)
    "eleven"
  elsif (i == 12)
    "twelve"
  elsif (i == 13)
    "thirteen"
  elsif (i == 14)
    "fourteen"
  elsif (i == 15)
    "fifteen"
  elsif (i == 16)
    "sixteen"
  elsif (i == 17)
    "seventeen"
  elsif (i == 18)
    "eighteen"
  elsif (i == 19)
    "ninteen"
  elsif (i < 30)
    "twenty" + (i == 20 ? "" : " " + create_number(i - 20))
  elsif (i < 40)
    "thirty" + (i == 30 ? "" : " " + create_number(i - 30))
  elsif (i < 50)
    "fourty" + (i == 40 ? "" : " " + create_number(i - 40))
  elsif (i < 60)
    "fifty" + (i == 50 ? "" : " " + create_number(i - 50))
  elsif (i < 70)
    "sixty" + (i == 60 ? "" : " " + create_number(i - 60))
  elsif (i < 80)
    "seventy" + (i == 70 ? "" : " " + create_number(i - 70))
  elsif (i < 90)
    "eighty" + (i == 80 ? "" : " " + create_number(i - 80))
  elsif (i < 100)
    "ninety" + (i == 90 ? "" : " " + create_number(i - 90))
  elsif (i < 200)
    "one hundred" + (i == 100 ? "" : " " + create_number(i - 100))
  elsif (i < 300)
    "two hundred" + (i == 200 ? "" : " " + create_number(i - 200))
  elsif (i < 400)
    "three hundred" + (i == 300 ? "" : " " + create_number(i - 300))
  elsif (i < 500)
    "four hundred" + (i == 400 ? "" : " " + create_number(i - 400))
  elsif (i < 600)
    "five hundred" + (i == 500 ? "" : " " + create_number(i - 500))
  elsif (i < 700)
    "six hundred" + (i == 600 ? "" : " " + create_number(i - 600))
  elsif (i < 800)
    "seven hundred" + (i == 700 ? "" : " " + create_number(i - 700))
  elsif (i < 900)
    "eight hundred" + (i == 800 ? "" : " " + create_number(i - 800))
  elsif (i < 1000)
    "nine hundred" + (i == 900 ? "" : " " + create_number(i - 900))
  else
    raise "i: #{i}"
  end
end

0.upto(999) do |i|
  puts create_number(i)
end
