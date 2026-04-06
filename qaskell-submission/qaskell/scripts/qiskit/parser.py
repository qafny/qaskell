import sys
import re

pauli_string = sys.stdin.read().strip()

term_pattern = r'([+-]?\d+(?:\.\d*)?(?:e[+-]?\d+)?)\s*\(\s*((?:[IXYZ]\(\w\)\s*@\s*)*[IXYZ]\(\w\))\s*\)'
matches = re.findall(term_pattern, pauli_string)

parsed_terms = []
qubit_labels = set()

for coeff, term in matches:
    ops = re.findall(r'([IXYZ])\((\w)\)', term)
    parsed_terms.append((float(coeff), ops))
    for _, label in ops:
        qubit_labels.add(label)

qubit_labels = sorted(qubit_labels)
label_to_index = {label: idx for idx, label in enumerate(qubit_labels)}

generated_lines = []
for coeff, ops in parsed_terms:
    if all(op == 'I' for op, _ in ops):
        continue
    
    op_index_pairs = [(op, label_to_index[q]) for op, q in ops]
    indices = [i for op, i in op_index_pairs]
    ops_only = [op for op, _ in op_index_pairs]

    labels = [label_to_index[q] for (_, q) in ops]
    paulis = [g for (g, _) in ops]

    non_identity = [(i, p) for i, p in zip(labels, paulis) if p != 'I']

    if all(p == 'Z' for (_, p) in non_identity):

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

        for run in all_runs:
            qubit_indices = sorted([idx for (_, idx) in run])

            if len(qubit_indices) == 1:
                i = qubit_indices[0]
                generated_lines.append(f"    qc.rz(-2 * gamma * {coeff} * dt, {i})")

            else:
                for i in range(len(qubit_indices) - 1):
                    ctrl = qubit_indices[i]
                    tgt = qubit_indices[i + 1]
                    generated_lines.append(f"    qc.cx({ctrl}, {tgt})")

                last_qubit = qubit_indices[-1]
                generated_lines.append(f"    qc.rz(-2 * gamma * {coeff} * dt, {last_qubit})")

                for i in range(len(qubit_indices) - 2, -1, -1):
                    ctrl = qubit_indices[i]
                    tgt = qubit_indices[i + 1]
                    generated_lines.append(f"    qc.cx({ctrl}, {tgt})")

    else:
        raise ValueError(f"Unsupported term: {ops}")


# Patch template
with open('scripts/qiskit/template.py', 'r') as f:
    template = f.read()

template = template.replace('n_qubits = 3', f'n_qubits = {len(qubit_labels)}')
if len(sys.argv) == 2:
    template = template.replace('is_clique = False', 'is_clique = True')
template = template.replace('# INSERT_RZZ_GATES_HERE', '\n'.join(generated_lines))

# Output final script
print(template)
