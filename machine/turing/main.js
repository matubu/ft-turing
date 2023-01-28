const BLANK = '.';
const VIRTUAL_BLANK = '_';

const virtual_char = (c) => {
	return c == BLANK ? VIRTUAL_BLANK : c;
};
const real_char = (c) => {
	return c == VIRTUAL_BLANK ? BLANK : c;
};

const ALLOWED_ALPH = [BLANK, '0', '1', '+'];
const VIRTUAL_ALPH = ALLOWED_ALPH.map(virtual_char);

const SPECIAL_ALPH = ['@', '|', ':', '_'];
const VIRTUAL_STATES = ['a', 'b', 'c', 'd'];
const ALPH = [...SPECIAL_ALPH, ...ALLOWED_ALPH, ...VIRTUAL_STATES];

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
	...goto('goto_main()', c => [c == '|' ? `set_pointer(state:a)` : 'goto_main()', c, 'RIGHT']),
	...gotos(VIRTUAL_STATES, state => {
		return {
			...goto(`set_pointer(state:${state})`, c => [`search(state:${state}, char:${c})`, '@', 'LEFT'], ALLOWED_ALPH),
			...gotos(ALLOWED_ALPH, char => {
				return {
					...goto(`search(state:${state}, char:${char})`, c => [
						c == ':'
							? `check_read(state:${state}, char:${char})`
							: `search(state:${state}, char:${char})`,
						c,
						"LEFT"
					], ALPH),
					...goto(`check_read(state:${state}, char:${char})`, c => [
						c == virtual_char(char)
							? `check_state(state:${state}, char:${char})`
							: `search(state:${state}, char:${char})`,
						c,
						"LEFT"
					], ALPH),
					...goto(`check_state(state:${state}, char:${char})`, c => [
						c == state
							? `get_transition()`
							: `search(state:${state}, char:${char})`,
						c,
						"LEFT"
					], ALPH),
				};
			}),
			...goto(`get_transition(state:${state})`, c => [`write(state:${state}, char:${real_char(c)})`, c, 'RIGHT'], VIRTUAL_ALPH),
			...gotos(ALLOWED_ALPH, char => {
				return {
					...goto(`write(state:${state}, char:${char})`, c => {
						if (c == '@') {
							return [state == VIRTUAL_STATES.at(-1) ? "HALT" : `set_pointer(state:${state})`, char, 'RIGHT'];
						}
						return [`write(state:${state}, char:${char})`, c, 'RIGHT'];
					}, ALPH),
				};
			}),
		}
	}),
	...goto('get_transition()', c => [`get_transition(state:${c})`, c, 'LEFT'], VIRTUAL_STATES),
};

const turing = {
	name: 'Turing',
	alphabet: ALPH,
	blank: '.',
	states: Object.keys(TRANSITIONS),
	initial: 'goto_main()',
	finals: ['HALT'],
	transitions: TRANSITIONS,
}

console.log(JSON.stringify(turing));