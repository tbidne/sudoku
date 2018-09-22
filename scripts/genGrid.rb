#!/usr/bin/ruby

print '['
id = 0
0.upto(8) do |col|
  0.upto(8) do |row|
    puts "  Domain.Cell #{id} #{col} #{row} #{(row + col) % 9 + 1} (-1) False,"
    id += 1
  end
end
puts ']'
print 'False'
