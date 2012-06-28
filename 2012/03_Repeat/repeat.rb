#!/usr/bin/env ruby

require 'curses'

include Curses

SEARCH_SIZE_UPDATE = 1
MIN_SIZE_UPDATE    = 2
MAX_SIZE_UPDATE    = 3
CANDIDATE_UPDATE   = 4

class String
  def largest_duplicate
    min = 1 and yield(MIN_SIZE_UPDATE, min)
    search_size = [100, [1, (length * 0.05).to_i].max].min and yield(SEARCH_SIZE_UPDATE, search_size)
    max = (length / 2).to_i and yield(MAX_SIZE_UPDATE, max)
    last_found_str = "" and yield(CANDIDATE_UPDATE, last_found_str)

    while (min <= max)
      dups = Hash.new
      found_dup = false

      0.upto(length - search_size - 1) do |i|
        str = self[i..(i + search_size - 1)]
        if (dups.include?(str))
          if (dups[str] + str.length - 1 < i)
            min = search_size + 1 and yield(MIN_SIZE_UPDATE, min)
            search_size = [search_size + 1, ((max + min) / 2).to_i].max and yield(SEARCH_SIZE_UPDATE, search_size)
            last_found_str = str and yield(CANDIDATE_UPDATE, last_found_str)

            found_dup = true
            break
          end
        else
          dups[str] = i
        end
      end

      unless (found_dup)
        max = search_size - 1 and yield(MAX_SIZE_UPDATE, max)
        search_size = [((max + min) / 2).to_i, search_size - 1].min and yield(SEARCH_SIZE_UPDATE, search_size)
      end
    end

    last_found_str
  end
end

def clear_window(window, color_pair)
  window.attron(color_pair) do
    1.upto(window.maxy - 2) do |i|
      window.setpos(i, 1)
      window.addstr(" " * (window.maxx() - 2))
    end
  end
end

unless (ARGV[0])
  puts "Expected filename argument"
  exit 1
end

longest_str = ""
begin
  init_screen
  curs_set(0)
  start_color
  init_pair(1, COLOR_RED, COLOR_WHITE)
  init_pair(2, COLOR_BLACK, COLOR_WHITE)
  noecho

  window = Window.new(0, 0, 0, 0)
  window.attron(color_pair(1)) { window.box(0, 0) }
  clear_window(window, color_pair(1))

  file_window = window.subwin(3, window.maxx() - 2, 1, 1)
  clear_window(file_window, color_pair(1))
  file_window.attron(color_pair(1)) do
    file_window.box(0, 0) 
    file_window.setpos(1, 2)
    file_window.addstr("#{ARGV[0]}")
  end

  stats_window = window.subwin(5, window.maxx() - 2, 4, 1)
  clear_window(stats_window, color_pair(1))
  stats_window.attron(color_pair(1)) do
    stats_window.box(0, 0) 
    stats_window.setpos(1, 2)
    stats_window.addstr("Target size:")
    stats_window.setpos(2, 2)
    stats_window.addstr("Minumum size:")
    stats_window.setpos(3, 2)
    stats_window.addstr("Maximum size:")
  end

  candidate_window = window.subwin(window.maxy() - 10, window.maxx() - 2, 9, 1)
  clear_window(candidate_window, color_pair(1))
  candidate_window.attron(color_pair(1)) { candidate_window.box(0, 0) }

  window.attron(color_pair(1)) do
    window.setpos(1, 2)
    window.addstr("Scanning file:")
    window.setpos(4, 2)
    window.addstr("Current parameters:")
    window.setpos(9, 2)
    window.addstr("Current Candidate:")
  end

  window.refresh

  longest_str = IO.read(ARGV[0]).largest_duplicate do |type, data|
    if (type == SEARCH_SIZE_UPDATE || type == MIN_SIZE_UPDATE || type == MAX_SIZE_UPDATE)
      stats_window.attron(color_pair(1)) do
        stats_window.setpos(type, 20)
        stats_window.addstr((" " * (stats_window.maxx() - 21)))
        stats_window.setpos(type, 20)
        stats_window.addstr(data.to_s)
      end
    elsif (type == CANDIDATE_UPDATE)
      clear_window(candidate_window, color_pair(1))
      candidate_window.attron(color_pair(1)) do
        line_number = 1
        data.split(/\n/).each do |s| 
          break if line_number >= candidate_window.maxy() - 1
          candidate_window.setpos(line_number, 2)
          candidate_window.addstr(s.length > candidate_window.maxx() - 3 ? s[0..(candidate_window.maxx() - 7)] + "..." : s)
          line_number += 1
        end
      end
    end
    window.refresh
    stats_window.refresh
    candidate_window.refresh
  end

  candidate_window.setpos(candidate_window.maxy() - 2, candidate_window.maxx() - 25)
  candidate_window.attron(color_pair(2)) { candidate_window.addstr("Press any key to quit") }
  candidate_window.refresh
  window.getch
ensure
  close_screen
end

puts "Longest Repeated String:"
puts longest_str
