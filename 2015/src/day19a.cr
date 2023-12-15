RESULTS = Set(String).new

transformations, input = File.read("txt/day19").strip.split("\n\n")
transformations.split("\n").map(&.split(" => ")).each do |(src, dst)|
  scan_at = 0
  until (scan_at = input.index(src, scan_at)).nil?
    RESULTS << input.sub((scan_at...(scan_at + src.size)), dst)
    scan_at += src.size
  end
end

p RESULTS.size
