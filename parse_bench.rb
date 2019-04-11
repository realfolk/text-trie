###############################################################
#                                                  ~ 2019.04.11
# |
# Copyright   :  Copyright (c) 2019 michael j. klein
# License     :  BSD3
# Maintainer  :  lambdamichael@gmail.com
#
# Parse and output markdown tables comparing benchmarks.
#
# To use:
#   stack test | tee test.log
#   ruby parse_bench.rb
#
# Output is written to `bench.md`.
###############################################################


module Enumerable
  def span(&block)
    self_enumerator = self.each_entry
    safe_next = ->(delay_me){
      begin
        self_enumerator.next
      rescue StopIteration
      end
    }
    truthy_output = Enumerator.new do |y|
      while (x = safe_next[nil]) && block[x]
        y << x
      end
    end.each.to_a
    rest_of_output = Enumerator.new do |y|
      while (x = safe_next[nil]) && block[x]
        y << x
      end
    end
    [ truthy_output,
      rest_of_output
    ]
  end

  def break(&block)
    self.span do |x|
      !block[x]
    end
  end
end

# p [1,2,3].span{|x| x < 1}.to_a.map(&:to_a)
# p [1,2,3].span{|x| x < 2}.to_a.map(&:to_a)
# p [1,2,3].span{|x| x < 3}.to_a.map(&:to_a)
# p [1,2,3].span{|x| x < 4}.to_a.map(&:to_a)

def parse_begin_end(name, input)
  if input&.[](0) && /^#{name}/ === input[0]
    output = input.drop(1)
    output.break do |line|
      /^End: #{name}$/ === line
    end
  else
    p name, input
  end
end

def parse_begin_ends(names, input)
  Enumerator.new do |y|
    names.each do |name|
      contents, input = parse_begin_end(name, input.to_a)
      y << contents
    end
  end
end

benchmark_lines = Enumerator.new do |y|
  File.open(File.join(__dir__, 'test.log'), 'r') do |f|
    reached_benchmarks = false
    f.each_line do |line|
      if reached_benchmarks
        line.chomp!
        unless line.empty?
          y << line
        end
      else
        if /FromListBench/ === line
          reached_benchmarks = true
          line.chomp!
          y << line
        end
      end
    end
  end
end

benchmark_names = ["FromListBench",
                   "FromListBench.Text",
                   "FromListBench.Text.Encode"]

bs_bench, t_bench, et_bench = parse_begin_ends(benchmark_names, benchmark_lines).to_a

# p bs_bench
# puts
# p t_bench
# puts
# p et_bench
# puts

def parse_benchmark(input)
  if name_line = input&.[](0)
    if /^\* (?<func>\w+) (?<bench>[\w\s]+): \.+$/ =~ name_line
      if result_line = input&.[](1)
        if /^ +(?<per_iteration>[\d.]+)ns per iteration \/ (?<per_second>[\d.]+) per second\.$/ =~ result_line
          result = {func: func,
                    bench: bench,
                    per_iteration: per_iteration.to_f,
                    per_second: per_second.to_f}
          return [result, input.drop(2)]
        end
      end
    end
  else
    return [nil, input]
  end
end

# puts
# p parse_benchmark(t_bench.to_a)

def parse_benchmarks(input)
  parsed_benchmark = nil
  Enumerator.new do |y|
    parsed_benchmark, input = parse_benchmark(input)
    while parsed_benchmark && input&.[](0)
      parsed_benchmark, input = parse_benchmark(input)
      if parsed_benchmark
        y << parsed_benchmark
      end
    end
  end
end

# p parse_benchmarks(t_bench).to_a
# puts
# p parse_benchmarks(bs_bench).to_a
# puts
# p parse_benchmarks(et_bench).to_a

File.open(File.join(__dir__,'bench.md'), 'w') do |f|
  tee_puts = Proc.new do |*args|
    f.puts(*args)
    puts(*args)
  end

  tee_puts["# text-trie benchmarks"]
  tee_puts[]

  tee_puts["We compare the per iteration/second timings of `Data.Trie.Text`,"]
  tee_puts["`Data.Trie` with Text encoded to `UTF16`, and `Data.Trie` with `Char8`."]
  tee_puts[]
  tee_puts["The associated values represent how many times faster"]
  tee_puts["`Data.Trie.Text` is than the given usage of `Data.Trie`."]
  tee_puts[]

  parse_benchmarks(t_bench).zip(parse_benchmarks(et_bench), parse_benchmarks(bs_bench)).map do |tb, etb, bsb|
    unless tb.values_at(:func, :bench) == bsb.values_at(:func, :bench)
      raise ["non-matching func, bench:",
             "tb.values_at(:func, :bench): #{tb.values_at(:func, :bench)}",
             "bsb.values_at(:func, :bench): #{bsb.values_at(:func, :bench)}"].join("\n")
    end
    unless tb.values_at(:func, :bench) == etb.values_at(:func, :bench)
      raise ["non-matching func, bench:",
             "tb.values_at(:func, :bench): #{tb.values_at(:func, :bench)}",
             "etb.values_at(:func, :bench): #{etb.values_at(:func, :bench)}"].join("\n")
    end

    tee_puts["## #{tb[:func]}: #{tb[:bench]}"]
    tee_puts[]

    tee_puts["Timing method           | Data.Trie (UTF16)    | Data.Trie (Char8)"]
    tee_puts["----------------------- | -------------------- | ---------------------"]

    tee_puts["Per iteration: #{tb[:per_iteration].to_s.ljust(8)} | " +
             "#{(etb[:per_iteration] / tb[:per_iteration]).to_s.ljust(20)} | " +
             "#{(bsb[:per_iteration] / tb[:per_iteration]).to_s.ljust(20)}"]
    tee_puts["Per second   : #{tb[:per_second].to_s.ljust(8)} | " +
             "#{(tb[:per_second] / etb[:per_second]).to_s.ljust(20)} | " +
             "#{(tb[:per_second] / bsb[:per_second]).to_s.ljust(20)}"]
    tee_puts[]
  end
end

