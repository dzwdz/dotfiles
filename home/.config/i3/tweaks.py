import i3ipc

def get_siblings(node):
	return [n for n in node.parent.nodes if n != node]

def find_swap_target(i3):
	focused = i3.get_tree().find_focused()

	siblings = get_siblings(focused.parent)
	if focused.parent.type == 'con' and siblings != []:
		return siblings[0]
	else:
		siblings = get_siblings(focused)
		for column in siblings:
			children = column.nodes
			if children != []:
				return children[0]

def on_binding(i3, event):
	cmd = event.binding.command
	if cmd == 'nop swap':
		target = find_swap_target(i3)
		if target:
			i3.command('swap container with con_id %s' % target.id)

	# we have monocle mode at home
	if cmd.startswith('focus') and not cmd.startswith('focus output') and i3.get_tree().find_focused().fullscreen_mode:
		i3.command('fullscreen toggle')
		i3.command(cmd)
		i3.command('fullscreen toggle')

i3 = i3ipc.Connection()
i3.on('binding', on_binding)
i3.main()
