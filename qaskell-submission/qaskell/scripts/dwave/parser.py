import sys
import re
import os

dir_path = os.path.dirname(os.path.realpath(__file__))

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
            if indices[0] == indices[1]:
                generated_lines.append(f"    h['{qubit_labels[indices[0]]}'] = {coeff}")
            else:
                generated_lines.append(f"    J[('{qubit_labels[0]}', '{qubit_labels[1]}')] = {coeff}")

    elif len(ops_only) == 3:

        if ops_only == ['Z', 'Z', 'I']:
            if indices[0] == indices[1]:
                generated_lines.append(f"    h['{qubit_labels[indices[0]]}'] = {coeff}")
            else:
                generated_lines.append(f"    J[('{qubit_labels[0]}', '{qubit_labels[1]}')] = {coeff}")
        elif ops_only == ['I', 'Z', 'Z']:
            if indices[1] == indices[2]:
                generated_lines.append(f"    h['{qubit_labels[indices[1]]}'] = {coeff}")
            else:
                generated_lines.append(f"    J[('{qubit_labels[1]}', '{qubit_labels[2]}')] = {coeff}")
        elif ops_only == ['Z', 'I', 'Z']:
            if indices[0] == indices[2]:
                generated_lines.append(f"    h['{qubit_labels[indices[0]]}'] = {coeff}")
            else:
                generated_lines.append(f"    J[('{qubit_labels[0]}', '{qubit_labels[2]}')] = {coeff}")

# Patch template
with open(f'{dir_path}/template.py', 'r') as f:
    template = f.read()

template = template.replace('# INSERT_HERE', '\n'.join(generated_lines))

# Output final script
print(template)
