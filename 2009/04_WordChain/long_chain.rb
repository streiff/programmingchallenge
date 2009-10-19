#!/usr/bin/ruby -w

MAX_TIME = 300

class String
    def adj?(word)
        0.upto(word.length - 1).inject(0) {|d, i| word[i] != self[i] ? d + 1 : d} == 1
    end
end

def read_words(filename, length)
    words = Array.new
    open(filename) {|file|
        file.each {|line| words.push(line.chomp) if line.length == length + 1 }
    }
    words
end

def find_chain(start_word, end_word, words, end_time)
    words = words.dup
    results = Array.new
    results.push(start_word)

    while (!results.empty?)
        word_sz = 0
        while (words.length != word_sz)
            word_sz = words.length

            (words.length - 1).downto(0) {|i|
                return [] if Time.new > end_time # time limit.
                is_start_end_chain = results.last == start_word && words[i] == end_word
                if (results.last.adj?(words[i]) && !is_start_end_chain)
                    results.push(words[i])
                    words.delete_at(i)
                    return results if (results.last == end_word)
                end
            }
        end
        results.pop
    end
    results
end

if (ARGV.size != 3 && ARGV.size != 4)
    puts "Usage: long_chain.rb START_WORD END_WORD DICT_FILE [TIME_IN_SECS]"
    puts "Note:  The program will stop trying to expand the chain when "
    puts "       TIME_IN_SECS is reached, and exit at the next safe point. "
    puts "       Default try time is #{MAX_TIME / 60} minutes"
    exit
end

start_word = ARGV[0]
end_word   = ARGV[1]
filename   = ARGV[2]
max_time   = ARGV[3] == nil ? MAX_TIME : ARGV[3].to_i

if (start_word.length != end_word.length)
    puts "start word and end word not the same length"
    exit
end

words = read_words(filename, start_word.length)

if (!words.any? {|s| s == start_word})
    puts "start word not found"
    exit
end

if (!words.any? {|s| s == end_word})
    puts "end word not found"
    exit
end

words.delete(start_word)
end_time = Time.new + max_time

found_words = start_word.adj?(end_word) ? 
        [start_word, end_word] : 
        find_chain(start_word, end_word, words, end_time)

if (found_words.empty?)
    found_words.each{|w| puts w}
    exit
end
words = words - found_words

# We have a chain, but not as long as we want it to be.
# Keep trying to insert words until the time is up, or
# we cannot find any new words to insert.
chain_sz = 0
while (chain_sz != found_words.length && Time.new < end_time)
    chain_sz = found_words.length
    (found_words.length-2).downto(0) {|i|
        words.push(found_words[i+1])
        new_words = find_chain(found_words[i], found_words[i+1], words, end_time)
        words.delete(found_words[i+1])
        if (new_words.length > 2)
            words = words - new_words
            found_words.insert(i+1, new_words[1..(new_words.length-2)])
            found_words.flatten!
        end
        break if Time.new > end_time
    }
end

found_words.each{|w| puts w}
