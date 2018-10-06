#!/usr/bin/ruby

id = 0
0.upto(8) do |col|
  0.upto(8) do |row|
    puts "(#{id}, 0, #{col}, #{row}, , , False),"
    id += 1
  end
end
