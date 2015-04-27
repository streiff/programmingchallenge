#!/usr/bin/env ruby

Coord = Struct.new(:x, :y)

class Bitmap

  def initialize(size)
    @map = Array.new(size) {Array.new(size) {' '}}
  end

  # Bresenham's Algorithm for drawing lines for a raster
  def draw_line(coord1, coord2, c)
    x1, y1 = coord1.x, coord1.y
    x2, y2 = coord2.x, coord2.y

    if (x1 == x2)
      (y1..y2).each {|y| @map[y][x1] = '#'}
      return
    elsif (y1 == y2)
      (x1..x2).each {|x| @map[y1][x] = '#'}
      return
    end

    steep = (y2 - y1).abs > (x2 - x1).abs
    if (steep)
      x1, y1 = y1, x1
      x2, y2 = y2, x2
    end

    if (x1 > x2)
      x1, x2 = x2, x1
      y1, y2 = y2, y1
    end

    deltax = x2 - x1
    deltay = (y2 - y1).abs
    error = deltax / 2
    ystep = y1 < y2 ? 1 : -1

    y = y1
    x1.upto(x2) do |x|
      pixel = steep ? [y, x] : [x, y]
      @map[y][x] = c
      error -= deltay
      if (error < 0)
        y += ystep
        error += deltax
      end
    end
  end

  def fill(coord, c)
    filling_c = @map[coord.y][coord.x]
    return if filling_c == c

    stack = Array.new
    stack.push coord

    until (stack.empty?)
      coord = stack.pop
      if (@map[coord.y][coord.x] == filling_c)
        @map[coord.y][coord.x] = c
        stack.push Coord[coord.x + 1, coord.y] if (coord.x < @map.size)
        stack.push Coord[coord.x - 1, coord.y] if (coord.x > 0)
        stack.push Coord[coord.x, coord.y + 1] if (coord.y < @map.size)
        stack.push Coord[coord.x, coord.y - 1] if (coord.y > 0)
      end
    end
    
  end

  def count(c)
    @map.inject(0) do |total, i| 
      total + i.inject(0) {|subtotal, j| subtotal + (j == c ? 1 : 0)}
    end
  end

  def to_s
    ret = ""
    @map.each do |line|
      ret = line.join('') + "\n" + ret
    end
    ret
  end
end

raise "Usage: #{$0} number" unless ARGV.length == 1
magic_number = ARGV[0].to_i 
bitmap = Bitmap.new(magic_number)
bitmap.draw_line(Coord[0, 0], Coord[0, magic_number - 1], '#')
bitmap.draw_line(Coord[0, 0], Coord[magic_number - 1, 0], '#')
bitmap.draw_line(Coord[magic_number - 1, 0], Coord[0, magic_number - 1], '#')
bitmap.fill(Coord[1,1], '#') if (magic_number > 3) # There is no fill under this size

puts bitmap
puts "Total for size #{magic_number}: #{bitmap.count('#')}"
