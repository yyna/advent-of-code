defmodule MyList do
  defp get_intersection(a, b) do
    m1 = MapSet.new(String.split(a, "", trim: true))
    m2 = MapSet.new(String.split(b, "", trim: true))
    MapSet.intersection(m1, m2) |> MapSet.to_list |> List.to_string
  end

  def get_compartments([a, b]) do
    get_intersection(a, b)
  end
  def get_compartments([ head | tail ]) do
    get_intersection(head, get_compartments(tail))
  end
end

split_rucksacks = fn
  (rucksack) -> (
    length = String.length(rucksack)
    {first, second} = String.split_at(rucksack, div(length, 2))
    [first, second]
  )
end

get_priority = fn
  (item) -> (
    ascii_number = :binary.first(item)
    if ascii_number > 96 do
      ascii_number - 96
    else
      ascii_number - 38
    end
  )
end

{:ok, input} = File.read("input")
input = String.split(input)

# Part 1
input
|> Enum.map(&(split_rucksacks.(&1)))
|> Enum.map(&(MyList.get_compartments(&1)))
|> Enum.map(&(get_priority.(&1)))
|> Enum.sum
|> IO.inspect

# Part 2
input
|> Enum.chunk_every(3)
|> Enum.map(&(MyList.get_compartments(&1)))
|> Enum.map(&(get_priority.(&1)))
|> Enum.sum
|> IO.inspect
