require 'matrix'

module SavePrincess
  PRINCESS_TOKEN = 'p'
  MARIO_TOKEN    = 'm'

  class << self
    def display_path_to_princess(max, matrix)
      puts "mario is at #{get_mario_location(max, matrix)}"
      puts "princess is at #{get_princess_location(max, matrix)}"
    end

    def get_mario_location(max, matrix)
      all_coords = (0..max).map {|i| (0..max).map {|j| [i, j] }}.flatten(1)
      all_coords.detect do |coords|
        matrix[*coords] == MARIO_TOKEN
      end
    end

    def get_princess_location(max, matrix)
      max_pos = max - 1
      possible_princess_coords = [
        [0, 0], [0, max_pos], [max_pos, 0], [max_pos, max_pos]
      ]

      possible_princess_coords.detect {|coords| matrix[*coords] == PRINCESS_TOKEN }
    end

    def main
      max    = ARGF.gets.chomp
      matrix = Matrix[*ARGF.read.lines.map {|l| l.chomp.chars }]
      display_path_to_princess(max.to_i, matrix)
    end
  end
end

SavePrincess.main
