user_str = input("> ")

for i in user_str:
	print(i + "    ", end="")

print("")

for i in user_str:
	print(hex(ord(i)) + " ", end="")

print("")

# this only kinda works since decimals don't always have three digits
for i in user_str:
	print(str(ord(i)) + "  ", end="")

print("")
