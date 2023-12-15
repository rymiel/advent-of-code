results = Set(String).new

transformations, output = File.read("txt/day19").strip.split("\n\n")
transformations = transformations.split("\n").map(&.split(" => "))

results << output

steps = 0

current = output
loop {
  transformations.each do |(dst, src)|
    scan_at = 0
    until (scan_at = current.index(src, scan_at)).nil?
      replaced = current.sub((scan_at...(scan_at + src.size)), dst)
      steps += 1
      if replaced == "e"
        p steps
        exit
      end
      current = replaced
      break
    end
  end
}
