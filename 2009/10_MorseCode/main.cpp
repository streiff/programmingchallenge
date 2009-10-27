#include <sstream>
#include <fstream>
#include <iostream>
#include <string.h>

// byte_array letters; // byte array of letters a..z
// int[26] letter_len; // length of bytes of each of the letters;
// 
// char[] input;
// skip if length(input) == 0 || not morse code
// 
// byte_array[] binput = convert_to_byte_array(input);
// char[][] words;
// set byte_ptr = 0;
// 
// find_words(byte_ptr, '');
// 
// fn find_words(byte_ptr, letters) {
//   for l (A..Z) {
//     bin_letter = get_letter(l);
//     if (bin_letter does not match input[byte_ptr]) {
//       reject;
//     }
//     new_ptr = byte_ptr + length(letter);
//     if (new_ptr is end of list) {
//       add(words, letters + l);
//     } else {
//       find_words(new_ptr, letters + l);
//     }
//   }
// }
// 

typedef unsigned int uint;

struct BitArray {
  uint length;
  uint* arr;
};

std::string* read_input();
BitArray* convert_to_bit_array(std::string*);
uint* create_letter_size_array();
uint* create_letter_array();
bool matches(BitArray*, int , uint, uint);
uint get_bit(uint, uint);
uint get_bit(BitArray*, uint);
uint set_bit(uint, uint, int);
void set_bit(uint*, uint, int);
void print_results(BitArray*, char*, uint, uint, uint*, uint*, bool, std::string&);
bool is_dictionary_word(std::string&, char*);

int main(int argc, char** argv) {
    bool do_lookup = argc > 1;
    const char* dict_filename = argc > 1 ? argv[1]: "";
    uint* letters = create_letter_array();
    uint* sizes = create_letter_size_array();
    std::string dict;
    char line[1024];

    if (do_lookup) {
      std::ifstream dict_file;
      dict_file.open(dict_filename, std::ios::in);
      if (!dict_file.is_open()) {
          std::cout << "file not found.\n";
          return -1;
      }
      while (!dict_file.eof()) {
          dict_file.getline(line, 1024);
          dict += line;
          dict += '\n';
      }
    }

    for(;;) {
        std::string* input = read_input();
        if (input == NULL) {
            return 0;
        }

        BitArray* bit_array = convert_to_bit_array(input);
        if (bit_array == NULL) {
            delete input;
            continue;
        }

        char* outputstr = new char[input->size() + 1];
        print_results(bit_array, outputstr, 0, 0, letters, sizes, do_lookup, dict);

        delete outputstr;
        delete input;
        delete[] bit_array->arr;
        delete bit_array;
    }
    delete[] letters;
    delete[] sizes;
}

void print_results(BitArray* bits, char* output, uint bitpos, uint outputpos, uint* letters, uint* sizes, bool do_lookup, std::string& dict) {
    for (int i = 0; i < 26; ++i) {
        if (matches(bits, bitpos, letters[i], sizes[i])) {
            output[outputpos] = i + 'A';
            output[outputpos + 1] = '\0';
            if (bitpos + sizes[i] == bits->length) {
                if (do_lookup && is_dictionary_word(dict, output)) {
                    std::cout << output << std::endl;
                } else if (!do_lookup) {
                    output[outputpos + 1] = '\0';
                    std::cout << output << std::endl;
                }
            } else {
                print_results(bits, output, bitpos + sizes[i], outputpos + 1, letters, sizes, do_lookup, dict);
            }
        }
    }
}

std::string* read_input() {
    std::string input;
    std::cin >> input;
    return std::cin.eof() ? NULL : new std::string(input);
}

/** Converts the given string into a byte array for processing.
    Note: The bit array is stored as a series of ints and the first int in 
    the  array is its length. 
*/
BitArray* convert_to_bit_array(std::string* input) {
    uint* arr = new uint[input->size()];
    uint size_of_int = sizeof(uint);
    uint size_of_int_in_bits = sizeof(uint) * 8;

    for (int i = 0; i < input->size(); ++i) {
        char letter = (*input)[i];
        if ('.' == letter) {
            set_bit(arr, i, 0);
        } else if ('-' == letter) {
            set_bit(arr, i, 1);
        } else {
            return NULL;
        }
    }

    BitArray* bits = new BitArray();
    bits->arr = arr;
    bits->length = input->size();
    return bits;
}

uint* create_letter_array() {
    uint bit_size = sizeof(uint) * 8;
    uint* letters = new uint[26];
    letters['A' - 'A'] = 0x01 << bit_size - 2;
    letters['B' - 'A'] = 0x08 << bit_size - 4;
    letters['C' - 'A'] = 0x0A << bit_size - 4;
    letters['D' - 'A'] = 0x04 << bit_size - 3;
    letters['E' - 'A'] = 0; 
    letters['F' - 'A'] = 0x02 << bit_size - 4;
    letters['G' - 'A'] = 0x06 << bit_size - 3;
    letters['H' - 'A'] = 0;
    letters['I' - 'A'] = 0;
    letters['J' - 'A'] = 0x07 << bit_size - 4;
    letters['K' - 'A'] = 0x05 << bit_size - 3;
    letters['L' - 'A'] = 0x04 << bit_size - 4;
    letters['M' - 'A'] = 0x03 << bit_size - 2;
    letters['N' - 'A'] = 0x02 << bit_size - 2;
    letters['O' - 'A'] = 0x07 << bit_size - 3;
    letters['P' - 'A'] = 0x06 << bit_size - 4;
    letters['Q' - 'A'] = 0x0D << bit_size - 4;
    letters['R' - 'A'] = 0x02 << bit_size - 3;
    letters['S' - 'A'] = 0;
    letters['T' - 'A'] = 0x01 << bit_size - 1;
    letters['U' - 'A'] = 0x01 << bit_size - 3;
    letters['V' - 'A'] = 0x01 << bit_size - 4;
    letters['W' - 'A'] = 0x03 << bit_size - 3;
    letters['X' - 'A'] = 0x09 << bit_size - 4;
    letters['Y' - 'A'] = 0x0B << bit_size - 4;
    letters['Z' - 'A'] = 0x0C << bit_size - 4;
    return letters;
}

uint* create_letter_size_array() {
    uint* sizes = new uint[26];
    sizes['A' - 'A'] = 2;
    sizes['B' - 'A'] = 4;
    sizes['C' - 'A'] = 4;
    sizes['D' - 'A'] = 3;
    sizes['E' - 'A'] = 1;
    sizes['F' - 'A'] = 4;
    sizes['G' - 'A'] = 3;
    sizes['H' - 'A'] = 4;
    sizes['I' - 'A'] = 2;
    sizes['J' - 'A'] = 4;
    sizes['K' - 'A'] = 3;
    sizes['L' - 'A'] = 4;
    sizes['M' - 'A'] = 2;
    sizes['N' - 'A'] = 2;
    sizes['O' - 'A'] = 3;
    sizes['P' - 'A'] = 4;
    sizes['Q' - 'A'] = 4;
    sizes['R' - 'A'] = 3;
    sizes['S' - 'A'] = 3;
    sizes['T' - 'A'] = 1;
    sizes['U' - 'A'] = 3;
    sizes['V' - 'A'] = 4;
    sizes['W' - 'A'] = 3;
    sizes['X' - 'A'] = 4;
    sizes['Y' - 'A'] = 4;
    sizes['Z' - 'A'] = 4;
    return sizes;
}

bool matches(BitArray* input, int input_pos, uint letter, uint letter_size) {
    for (uint i = 0; i < letter_size; ++i, ++input_pos) {
        if (input_pos > input->length) {
            // too far. no dice.
            return false;
        }

        // check the bit.
        int input_bit = get_bit(input, input_pos);
        int letter_bit = get_bit(letter, i);

        if (input_bit != letter_bit) {
            return false;
        }
    }
    return true;
}

bool is_dictionary_word(std::string& dict, char* w) {
    std::istringstream sstream(dict);
    std::string line;
    std::string word(w);

    while (std::getline(sstream, line)) {
        if (strcasecmp(word.c_str(), line.c_str()) == 0) {
             return true;
         }
    }
    return false;
}

uint get_bit(uint num, uint bit_num) {
    return (num >> (sizeof(uint) * 8 - bit_num - 1)) & 1;
}

uint get_bit(BitArray* arr, uint bit_num) {
    uint byte_num = bit_num / (sizeof(uint) * 8);
    bit_num %= (sizeof(uint) * 8);
    return get_bit(arr->arr[byte_num], bit_num);
}

uint set_bit(uint num, uint bit_num, int val) {
    uint mask;
    if (val == 0) {
        mask = ~(1 << (sizeof(uint) * 8 - bit_num - 1));
        num &= mask;
    } else {
        mask = 1 << (sizeof(uint) * 8 - bit_num - 1);
        num |= mask;
    }
    return num;
    
}

void set_bit(uint* arr, uint bit_num, int val) {
    uint idx = bit_num / (sizeof(uint) * 8);
    uint value = arr[idx];
    value = set_bit(value, bit_num, val);
    arr[idx] = value;
}
