# template.py - Adiabatic Quantum Evolution
from qiskit import QuantumCircuit, QuantumRegister, ClassicalRegister, transpile
from qiskit.circuit.library import RZGate, RZZGate, RXXGate, RYYGate
from qiskit.providers.basic_provider import BasicSimulator
from qiskit.visualization import plot_histogram
import numpy as np
import matplotlib.pyplot as plt

n_qubits = 3
T = 100.0  # Total evolution time
trotter_steps = 500  # Number of Trotter steps
dt = T / trotter_steps

qr = QuantumRegister(n_qubits)
cr = ClassicalRegister(n_qubits, name='cr')
qc = QuantumCircuit(qr, cr)


for i in range(n_qubits):
    qc.h(i)

for step in range(trotter_steps):
    t = step / trotter_steps
    gamma = t
    beta = 1-t

# INSERT_RZZ_GATES_HERE
    
    for i in range(n_qubits):
        qc.rx(2 * beta * dt, i)

qc.measure(range(n_qubits), range(n_qubits))

simulator = BasicSimulator()

compiled_circuit = transpile(qc, simulator)
job = simulator.run(compiled_circuit, shots=1024)
result = job.result()
counts = result.get_counts()

# Sort by count and show top results
sorted_counts = sorted(counts.items(), key=lambda x: x[1], reverse=True)

top5 = sorted(counts.items(), key=lambda x: x[1], reverse=True)[:5]
print("Top 5 counts:")
print(top5)

# histogram
print("Plotting histogram")
plot_histogram(counts)
