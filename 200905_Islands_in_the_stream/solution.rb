#!/usr/bin/ruby -w

class Point
  attr_reader :x
  attr_reader :y

  def initialize(x, y)
    @x = x
    @y = y
  end

  def adj?(p)
    (-1..1).member?(p.x - @x) && (-1..1).member?(p.y - @y)
  end

  def connected_points(l)
    ret_list = Array.new
    ret_list.push(self)
    sz = 0
    while (sz != ret_list.size)
      sz = ret_list.size
      l.each {|i|
        ret_list.push(i) if !ret_list.include?(i) && ret_list.any? {|j| j.adj?(i) }
      }
    end
    ret_list
  end
end

class Map
  attr_reader :map
  attr_reader :sizes

  def Map.from_file(filename)
    a = Array.new
    File.open(filename) {|file| file.each {|line| a.push(line.chomp)} }
    Map.new(a)
    rescue
    nil
  end

  def initialize(map)
    @map = map
    process
  end

  def land?(x, y)
    @map[y][x].chr == "+"
  end

  def to_s
    contenent = "contenent" + (@sizes.size != 1 ? "s" : "")
    "#{map.join("\n")}\n\n#{@sizes.length} #{contenent}\n#{@sizes.join("\n")}\n\n"
  end

  private
  def process
    @sizes = Array.new

    points = Array.new
    0.upto(@map.length - 1) {|y|
      0.upto(@map[y].length - 1) { |x|
        points.push(Point.new(x, y)) if land?(x, y)
      }
    }

    while (!points.empty?)
      point = points.pop
      landmass = point.connected_points(points)
      points -= landmass
      @sizes.push(landmass.length)
    end
  end
end

while (true)
  print "File (type 'quit' to exit): "
  filename = gets.chomp
  exit if filename == "quit"
  map = Map.from_file(filename)
  puts map == nil ? "Error reading file..." : map
end
