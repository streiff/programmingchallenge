#!/usr/bin/env ruby

UNKNOWN = 1
HIT_ACCOUNTED_FOR = 2
HIT_UNACCOUNTED_FOR = 3
MISS = 4

SHIP_D = 2
SHIP_S = 3
SHIP_C = 4
SHIP_B = 5

SHIP_TYPES = {
  "D", SHIP_D,
  "S", SHIP_S,
  "C", SHIP_C,
  "B", SHIP_B
}

class Coord
  attr_reader :row, :col
  
  def initialize(row, col)
    @row = row
    @col = col
  end

  def Coord.from_s(s)
    row = s[0] - "A"[0]
    col = s[1] - "0"[0]
    Coord.new(row, col)
  end

  def each(c2)
    raise "Must be horizontal or vertical" if @row != c2.row && @col != c2.col
    if @row == c2.row
      [@col, c2.col].min.upto([@col, c2.col].max) do |col|
        yield Coord.new(@row, col)
      end
    else
      [@row, c2.row].min.upto([@row, c2.row].max) do |row|
        yield Coord.new(row, @col)
      end
    end
  end

  def to_s
    str = "A"
    @row.times { str.succ! }
    str + @col.to_s
  end

  def ==(c)
    c.row == @row && c.col == @col
  end
end

class State
  attr_reader :coord

  attr_accessor :possibility_cnt
  attr_accessor :status

  def initialize(coord)
    @coord = coord
    @status = UNKNOWN
    @possibility_cnt = 0
  end

  def to_s
    sprintf "[%s %s %3d]", coord.to_s, status, possibility_cnt
  end

  def status_to_s
    if (status == UNKNOWN)
      "."
    elsif (status == HIT_ACCOUNTED_FOR)
      "X"
    elsif (status == HIT_UNACCOUNTED_FOR)
      "x"
    elsif (status == MISS)
      "O"
    else
      raise "Unknown status"
    end
  end
end

class Ship
  attr_reader :start_coord
  attr_reader :end_coord

  def initialize(ship_type, start_coord, end_coord)
    @ship_type = ship_type
    @start_coord = start_coord
    @end_coord = end_coord
  end

  def Ship.random_location(ship_type)
    while(1)
      start_coord = Coord.new(rand(10), rand(10))

      dir = rand(4)
      ship_size = SHIP_TYPES[ship_type]
      if (dir == 0)
        end_coord = Coord.new(start_coord.row + ship_size - 1, start_coord.col)
      elsif (dir == 1)
        end_coord = Coord.new(start_coord.row - ship_size + 1, start_coord.col)
      elsif (dir == 2)
        end_coord = Coord.new(start_coord.row, start_coord.col + ship_size - 1)
      else
        end_coord = Coord.new(start_coord.row, start_coord.col - ship_size + 1)
      end

      if ((0..9).include?(end_coord.row) && (0..9).include?(end_coord.col))
        return Ship.new(ship_type, start_coord, end_coord)
      end
    end
  end

  def overlaps?(ships)
    ships.each do |s|
      @start_coord.each(@end_coord) do |c1|
        s.start_coord.each(s.end_coord) do |c2|
          return true if c1 == c2
        end
      end
    end
    false
  end

  def each_coord
    @start_coord.each(@end_coord) {|c| yield c}
  end

  def to_s
    "#{@ship_type} #{@start_coord} #{@end_coord}"
  end

  def Ship.from_s(s)
    details = s.split(/\s/)
    return nil if details.length != 3
    Ship.new(details[0], Coord.from_s(details[1]), Coord.from_s(details[2]))
  end
end

class Grid
  def initialize(past_ships)
    @past_ships = past_ships
    @last_hit_shots = []
    @grid = []
    0.upto(9) do |row|
      @grid[row] = []
      0.upto(9) do |col|
        @grid[row] << State.new(Coord.new(row, col))
      end
    end
    @unhit_ships = [ SHIP_D, SHIP_S, SHIP_S, SHIP_C, SHIP_B ]
    update_possibility_cnts
  end

  def mark(coord, status)
    @grid[coord.row][coord.col].status = status
    @last_hit_shots << coord if status == HIT_UNACCOUNTED_FOR
    update_possibility_cnts
  end

  def sink(ship_type)
    if (@last_hit_shots.length == ship_type)
        @last_hit_shots.each do |coord|
          @grid[coord.row][coord.col].status = HIT_ACCOUNTED_FOR
        end
        @last_hit_shots.clear
    else
        sinking_coord = @last_hit_shots.pop
        h1_coord = sinking_coord
        h2_coord = sinking_coord
        v1_coord = sinking_coord
        v2_coord = sinking_coord
        
        (ship_type - 1).times do
          if (h1_coord.col > 0 && @grid[h1_coord.row][h1_coord.col - 1].status == HIT_UNACCOUNTED_FOR)
            h1_coord = Coord.new(h1_coord.row, h1_coord.col - 1)
          elsif (h2_coord.col < 9 && @grid[h1_coord.row][h2_coord.col + 1].status == HIT_UNACCOUNTED_FOR)
            h2_coord = Coord.new(h2_coord.row, h2_coord.col + 1)
          end

          if (v1_coord.row > 0 && @grid[v1_coord.row - 1][v1_coord.col].status == HIT_UNACCOUNTED_FOR)
            v1_coord = Coord.new(v1_coord.row - 1, v1_coord.col)
          elsif (v1_coord.row < 9 && @grid[v1_coord.row + 1][v1_coord.col].status == HIT_UNACCOUNTED_FOR)
            v2_coord = Coord.new(v2_coord.row + 1, v2_coord.col)
          end
        end

        if (h2_coord.col - h1_coord.col == ship_type - 1)
          ship_type.times do |i|
            c = Coord.new(h1_coord.row, h1_coord.col + i)
            mark(c, HIT_ACCOUNTED_FOR)
            @last_hit_shots.delete(c)
          end
        elsif (v2_coord.row - v1_coord.row == ship_type - 1)
          ship_type.times do |i|
            c = Coord.new(v1_coord.row + i, v1_coord.col)
            mark(c, HIT_ACCOUNTED_FOR)
            @last_hit_shots.delete(c)
          end
        end
    end

    @unhit_ships.delete_at(@unhit_ships.find_index(ship_type))
    update_possibility_cnts
  end

  def each_cell
    @grid.each { |row| row.each { |cell| yield cell }}
  end

  def update_possibility_cnts
    each_cell { |cell| cell.possibility_cnt = 0 }

    # Update it based off of the unknown ships first
    @unhit_ships.each do |ship|
      0.upto(9) do |row|
        0.upto(9 - ship + 1) do |col|
          all_unknown = true
          0.upto(ship - 1) {|i| all_unknown = false if @grid[row][col + i].status != UNKNOWN}
          0.upto(ship - 1) {|i| @grid[row][col + i].possibility_cnt += 1} if (all_unknown)
        end
      end

      0.upto(9 - ship + 1) do |row|
        0.upto(9) do |col|
          all_unknown = true
          0.upto(ship - 1) {|i| all_unknown = false if @grid[row + i][col].status != UNKNOWN}
          0.upto(ship - 1) {|i| @grid[row + i][col].possibility_cnt += 1} if (all_unknown)
        end
      end
    end

    # Slightly bump it up based on opponent's last ship locations
    @past_ships.each do |ship|
      ship.each_coord do |coord|
        if (@grid[coord.row][coord.col].status == UNKNOWN && @grid[coord.row][coord.col].possibility_cnt > 0 && @grid[coord.row][coord.col].possibility_cnt < 100)
          @grid[coord.row][coord.col].possibility_cnt += 1
        end
      end
    end

    # Add in information based off of unaccounted for hits
    unaccounted_coords = hit_unaccounted_for_coords
    if(vertically_aligned(unaccounted_coords))
      unaccounted_coords.each do |coord| 
        row = coord.row
        col = coord.col

        @grid[row + 1][col].possibility_cnt += 1000 if (row < 9 && @grid[row + 1][col].status == UNKNOWN)
        @grid[row - 1][col].possibility_cnt += 1000 if (row > 0 && @grid[row - 1][col].status == UNKNOWN)
        @grid[row][col + 1].possibility_cnt += 500 if (col < 9 && @grid[row][col + 1].status == UNKNOWN)
        @grid[row][col - 1].possibility_cnt += 500 if (col > 0 && @grid[row][col - 1].status == UNKNOWN)
      end
    elsif(horizontally_aligned(unaccounted_coords))
      unaccounted_coords.each do |coord| 
        row = coord.row
        col = coord.col

        @grid[row + 1][col].possibility_cnt += 500 if (row < 9 && @grid[row + 1][col].status == UNKNOWN)
        @grid[row - 1][col].possibility_cnt += 500 if (row > 0 && @grid[row - 1][col].status == UNKNOWN)
        @grid[row][col + 1].possibility_cnt += 1000 if (col < 9 && @grid[row][col + 1].status == UNKNOWN)
        @grid[row][col - 1].possibility_cnt += 1000 if (col > 0 && @grid[row][col - 1].status == UNKNOWN)
      end
    else
      unaccounted_coords.each do |coord| 
        row = coord.row
        col = coord.col
        @grid[row + 1][col].possibility_cnt += 500 if (row < 9 && @grid[row + 1][col].status == UNKNOWN)
        @grid[row - 1][col].possibility_cnt += 500 if (row > 0 && @grid[row - 1][col].status == UNKNOWN)
        @grid[row][col + 1].possibility_cnt += 500 if (col < 9 && @grid[row][col + 1].status == UNKNOWN)
        @grid[row][col - 1].possibility_cnt += 500 if (col > 0 && @grid[row][col - 1].status == UNKNOWN)
      end
    end
  end

  def hit_unaccounted_for_coords
    coords = []
    each_cell {|cell| coords << cell.coord if cell.status == HIT_UNACCOUNTED_FOR}
    coords
  end

  def highest_probability_coords
    high = 0
    coords = []
    each_cell do |cell| 
      if cell.possibility_cnt == high
        coords << cell.coord if cell.status == UNKNOWN
      elsif cell.possibility_cnt > high
        high = cell.possibility_cnt
        coords.clear
        coords << cell.coord if cell.status == UNKNOWN
      end
    end
    coords
  end

  def to_debug_s
    s = ""
    0.upto(9) do |y|
      0.upto(9) do |x|
        s += @grid[y][x].to_s + " "
      end
      s += "\n"
    end
    s
  end

  def to_s
    s = "  0 1 2 3 4 5 6 7 8 9\n"
    0.upto(9) do |row|
        s += Coord.new(row, 0).to_s[0..0] + " "
      0.upto(9) do |col|
        s += @grid[row][col].status_to_s + " "
      end
      s += "\n"
    end
    s
  end
end

def vertically_aligned(unaccounted_coords)
  return false if unaccounted_coords.size <= 1

  col = unaccounted_coords[0].col
  unaccounted_coords.each { |coord| return false if col != coord.col }
  return true
end

def horizontally_aligned(unaccounted_coords)
  return false if unaccounted_coords.size <= 1

  row = unaccounted_coords[0].row
  unaccounted_coords.each { |coord| return false if row != coord.row }
  return true
end

def place_ships
  ships = [Ship.random_location("B")]

  ["C", "S", "S", "D"].each do |ship_type|
    ship = Ship.random_location(ship_type)
    while (ship.overlaps?(ships))
      ship = Ship.random_location(ship_type)
    end
    ships << ship
  end

  ships.each {|s| $stdout.puts s }

  $stdout.flush
end

def get_ship_type(type)
  return SHIP_TYPES[type]
end

def read_boat_file(file)
  boats = []
  return boats unless File.exists?(file)

  File.open(file, "r") do |f|
    s = f.gets
    s.chomp! unless s == nil
    
    while (s != nil)
      ship = Ship.from_s(s)
      boats << ship if ship != nil

      s = f.gets
      s.chomp! unless s == nil
    end
  end

  boats
end


enemy_name = ARGV[0]
past_ships = read_boat_file("#{enemy_name}.boats")

place_ships

exit if $stdin.gets.chomp != "accept"

exit_commands = ["win", "loss", "tie", "exit"]
command = $stdin.gets.chomp

grid = Grid.new(past_ships)


while (!exit_commands.include?(command))
  if (command == "fire")
    last_coord = grid.highest_probability_coords[rand(grid.highest_probability_coords.size)]
    $stdout.puts last_coord.to_s
    $stdout.flush
  elsif (command == "miss")
    grid.mark(last_coord, MISS)
  elsif (command == "hit")
    grid.mark(last_coord, HIT_UNACCOUNTED_FOR)
  elsif (command.start_with?("sink"))
    grid.sink(get_ship_type(command.split(/ /)[1]))
  elsif (command.start_with?("incoming"))
  elsif (command == "debug")
    $stdout.puts grid.to_s
    $stdout.puts grid.to_debug_s
  end
  
  command = $stdin.gets.chomp
end

unless ("exit" == command)
  open("#{enemy_name}.boats", "a") do |f|
    s = $stdin.gets
    s = s.chomp if s != nil
    while (s != nil)
      ship = Ship.from_s(s)
      f.puts s unless ship == nil
      s = $stdin.gets
      s = s.chomp if s != nil
    end
  end
end


