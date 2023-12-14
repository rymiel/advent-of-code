require "json"

def recursive_sum(json : JSON::Any) : Int64
  case v = json.raw
  in Int64   then v
  in String  then 0i64
  in Bool    then 0i64
  in Nil     then 0i64
  in Float64 then 0i64
  in Array   then v.sum { |i| recursive_sum i }
  in Hash
    return 0i64 if v.values.any? &.== "red"

    v.values.sum { |i| recursive_sum i }
  end
end

p recursive_sum JSON.parse(File.open("txt/day12"))
