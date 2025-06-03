import sys
import re

# Read input from stdin
pauli_string = sys.stdin.read().strip()

# Match terms like: 0.25 (Z(a) @ I(b) @ Z(c))
term_pattern = r'([+-]?\d+(?:\.\d*)?)\s*\(\s*((?:[IXYZ]\(\w\)\s*@\s*)*[IXYZ]\(\w\))\s*\)'
matches = re.findall(term_pattern, pauli_string)

# Parse ops and collect all qubit labels
parsed_terms = []
qubit_labels = set()

for coeff, term in matches:
    ops = re.findall(r'([IXYZ])\((\w)\)', term)
    parsed_terms.append((float(coeff), ops))
    for _, label in ops:
        qubit_labels.add(label)

# Assign integer indices to qubit labels
qubit_labels = sorted(qubit_labels)
label_to_index = {label: idx for idx, label in enumerate(qubit_labels)}

# Process each term
generated_lines = []

for coeff, ops in parsed_terms:
    if all(op == 'I' for op, _ in ops):
        continue

    # Map ops to indices
    op_index_pairs = [(op, label_to_index[q]) for op, q in ops]
    indices = [i for op, i in op_index_pairs]
    ops_only = [op for op, _ in op_index_pairs]

    if len(ops_only) == 2:
        if ops_only == ['Z', 'Z']:
            i, j = indices
            generated_lines.append(f"    qc.rzz(-2 * gamma * {coeff} * (T / trotter_steps), {i}, {j})")
        # else:
        #     for op, i in zip(ops_only, indices):
        #         if op == 'Z':
        #             generated_lines.append(f"    qc.rz({coeff}, {i})")

    elif len(ops_only) == 3:
        (op1, i), (op2, j), (op3, k) = op_index_pairs

        # Single-qubit RZ cases
        # for op, idx in [(op1, i), (op2, j), (op3, k)]:
        #     if op == 'Z' and [op1, op2, op3].count('Z') == 1:
        #         generated_lines.append(f"    qc.rz({coeff}, {idx})")
        #         break

        # Two-qubit RZZ cases
        if [op1, op2, op3] == ['Z', 'Z', 'I']:
            generated_lines.append(f"    qc.rzz(-2 * gamma * {coeff} * (T / trotter_steps), {i}, {j})")
        elif [op1, op2, op3] == ['I', 'Z', 'Z']:
            generated_lines.append(f"    qc.rzz(-2 * gamma * {coeff} * (T / trotter_steps), {j}, {k})")
        elif [op1, op2, op3] == ['Z', 'I', 'Z']:
            generated_lines.append(f"    qc.rzz(-2 * gamma * {coeff} * (T / trotter_steps), {i}, {k})")

        # else:
        #     raise Exception(f"Unsupported 3-op term: {ops}")

    # Other term lengths: skip

# Patch template
with open('scripts/qiskit/template.py', 'r') as f:
    template = f.read()

template = template.replace('n_qubits = 3', f'n_qubits = {len(qubit_labels)}')
template = template.replace('# INSERT_RZZ_GATES_HERE', '\n'.join(generated_lines))

# Output final script
print(template)
