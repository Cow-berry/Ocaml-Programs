while true do
  if Graphics.key_pressed() then
  (
      let k = Graphics.read_key;
      print_char k;
  );;
done
