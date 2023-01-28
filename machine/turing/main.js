const BLANK = '.';
const ALLOWED_ALPH = ['0', '1', '+'];
const ALPH = [BLANK, '@', '|', ...ALLOWED_ALPH];
const VIRTUAL_ALPH = [...ALLOWED_ALPH, BLANK];
const VIRTUAL_STATES = ['a', 'b', 'c'];

const goto = (name, func, arr = ALPH) => {
	return ({
		[name]: arr.map(c => {
			const [to_state, write, action] = func(c);
			return {
				read: c,
				to_state,
				write,
				action
			};
		})
	});
};

const gotos = (arr, func) => {
	return Object.fromEntries(arr.map((state) => {
		return Object.entries(func(state));
	}).flat());
};

const TRANSITIONS = {
	'HALT': [],
	...goto('goto_main', c => [c == '|' ? `goto_conf_${VIRTUAL_STATES[0]}` : 'goto_main', c, 'RIGHT']),
	
	...gotos(VIRTUAL_STATES, state => {
		return {
			...goto(`goto_conf_${state}`, c => [`goto_conf_${state}${c}`, c, "LEFT"], VIRTUAL_ALPH),
			...gotos(VIRTUAL_ALPH, c => {
				return {
					...goto(`goto_conf_${state}${c}`, c2 => [`goto_conf_${state}${c}`, c2, "LEFT"]),
					...goto(`get_conf_${state}${c}`, c2 => [`goto_conf_${state}${c}`, c2, "LEFT"])
				};
			})
		}
	}),
	// ...goto('goto_config', '|', 'a', 'LEFT'),
	// ...goto('goto_pointer', '@', 'a'),
};

const turing = {
	name: 'Turing',
	alphabet: [BLANK, ...ALLOWED_ALPH],
	blank: '.',
	states: Object.keys(TRANSITIONS),
	initial: ['goto_main'],
	finals: ['HALT'],
	transition: TRANSITIONS,
}

console.log(turing);