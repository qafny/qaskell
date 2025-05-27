import sys
import re

# Read input from stdin
pauli_string = sys.stdin.read().strip()

# Matches terms like: 0.5 (Z(a) ⊗ Z(b)) or I(a) ⊗ I(b)
pattern = r"([+-]?\d+(?:\.\d*)?)\s*\(\s*([IXYZ])\((\w)\)\s*@\s*([IXYZ])\((\w)\)\s*\)"
matches = re.findall(pattern, pauli_string)

# Step 1: Collect all qubit labels
qubit_labels = sorted(set([q1 for _, _, q1, _, _ in matches] + [q2 for _, _, _, _, q2 in matches]))
label_to_index = {label: idx for idx, label in enumerate(qubit_labels)}

# Step 2: Filter only Z⊗Z terms and convert to indices
valid_terms = []
for coeff, op1, q1, op2, q2 in matches:
    if op1 == 'Z' and op2 == 'Z':
        i, j = label_to_index[q1], label_to_index[q2]
        valid_terms.append((float(coeff), i, j))
    #else:
    #    print(f"# Skipped term: {coeff} ({op1}({q1}) ⊗ {op2}({q2}))", file=sys.stderr)

# Step 3: Generate RZZ lines
rzz_lines = [
    f"    qc.rzz(-2 * gamma * {coeff} * (T / trotter_steps), {i}, {j})"
    for coeff, i, j in valid_terms
]

# Step 4: Read and patch the template
with open('scripts/template.py', 'r') as f:
    template = f.read()

# Step 5: Replace placeholders
template = template.replace('n_qubits = 3', f'n_qubits = {len(qubit_labels)}')
template = template.replace('# INSERT_RZZ_GATES_HERE', '\n'.join(rzz_lines))

# Step 6: Output the final script
print(template)
