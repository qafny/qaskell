import sys
import re

# Read input from stdin
pauli_string = sys.stdin.read().strip()

# Match terms like: 0.25 (Z(a) @ I(b) @ Z(c))
#term_pattern = r'([+-]?\d+(?:\.\d*)?)\s*\(\s*((?:[IXYZ]\(\w\)\s*@\s*)*[IXYZ]\(\w\))\s*\)'
term_pattern = r'([+-]?\d+(?:\.\d*)?(?:e[+-]?\d+)?)\s*\(\s*((?:[IXYZ]\(\w\)\s*@\s*)*[IXYZ]\(\w\))\s*\)'
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

    labels = [label_to_index[q] for (_, q) in ops]
    paulis = [g for (g, _) in ops]

    non_identity = [(i, p) for i, p in zip(labels, paulis) if p != 'I']

    if all(p == 'Z' for (_, p) in non_identity):

        # Map (index in qubit list, position in ops list)
        ops_with_indices = [(label_to_index[q], p) for p, q in ops]

        current_run = []
        all_runs = []

        for pos, (idx, p) in enumerate(ops_with_indices):
            if p == 'Z':
                current_run.append((pos, idx))
            else:
                if current_run:
                    all_runs.append(current_run)
                    current_run = []
        if current_run:
            all_runs.append(current_run)

        # Process each Z run independently
        for run in all_runs:
            qubit_indices = [idx for (_, idx) in run]

            if len(qubit_indices) == 1:
                i = qubit_indices[0]
                generated_lines.append(f"    qc.rz(-2 * gamma * {coeff}, {i})  # from: {ops}")

            elif len(qubit_indices) == 2:
                i, j = qubit_indices
                generated_lines.append(f"    qc.rzz(-2 * gamma * {coeff}, {i}, {j})  # from: {ops}")

            elif len(qubit_indices) > 2:
                qubit_indices = sorted(qubit_indices)
                ladder_pairs = list(zip(qubit_indices[:-1], qubit_indices[1:]))

                for a, b in ladder_pairs[:-1]:
                    generated_lines.append(f"    qc.cx({a}, {b})")

                i, j = ladder_pairs[-1]
                generated_lines.append(f"    qc.rzz(-2 * gamma * {coeff}, {i}, {j})  # from: {ops}")

                for a, b in reversed(ladder_pairs[:-1]):
                    generated_lines.append(f"    qc.cx({a}, {b})")

    elif all(p == 'X' for (_, p) in non_identity):
        if len(non_identity) == 1:
            i = non_identity[0][0]
            generated_lines.append(f"    qc.rx(-2 * gamma * {coeff}, {i})  # from: {ops}")
        elif len(non_identity) == 2:
            i, j = non_identity[0][0], non_identity[1][0]
            generated_lines.append(f"    qc.rxx(-2 * gamma * {coeff}, {i}, {j})  # from: {ops}")
        else:
            raise ValueError(f"Unsupported X tensor size: {ops}")
            
    elif all(p == 'Y' for (_, p) in non_identity):
        if len(non_identity) == 1:
            i = non_identity[0][0]
            generated_lines.append(f"    qc.ry(-2 * gamma * {coeff}, {i})  # from: {ops}")
        elif len(non_identity) == 2:
            i, j = non_identity[0][0], non_identity[1][0]
            generated_lines.append(f"    qc.ryy(-2 * gamma * {coeff}, {i}, {j})  # from: {ops}")
        else:
            raise ValueError(f"Unsupported Y tensor size: {ops}")

    else:
        raise ValueError(f"Unsupported term: {ops}")


# Patch template
with open('scripts/qiskit/template.py', 'r') as f:
    template = f.read()

template = template.replace('n_qubits = 3', f'n_qubits = {len(qubit_labels)}')
if len(sys.argv) == 2:
    flag = sys.argv[1]
    if flag == 'is_clique':
        template = template.replace('is_clique = False', 'is_clique = True')
    elif flag == 'is_k_clique':
        template = template.replace('is_k_clique = False', 'is_k_clique = True')
    elif flag == 'is_hamiltonianCycle':
        template = template.replace('is_hamiltonianCycle = False', 'is_hamiltonianCycle = True')
    elif flag == 'is_tsp':
        template = template.replace('is_tsp = False', 'is_tsp = True')
template = template.replace('# INSERT_RZZ_GATES_HERE', '\n'.join(generated_lines))

# Output final script
print(template)
