require "digest/md5"

(0..).each do |i|
  digest = Digest::MD5.new
  digest << File.read("txt/day4").strip.to_slice
  digest << i.to_s.to_slice
  result = digest.hexfinal
  if result.starts_with?("000000")
    p i
    break
  end
end
