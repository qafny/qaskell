import dwave.inspector
from dwave.system import EmbeddingComposite, DWaveSampler


def run_on_dwave(Q: dict, label: str, chainstrength: float, numruns: int):
    sampler = EmbeddingComposite(DWaveSampler())
    response = sampler.sample_qubo(Q,
                                   chain_strength=chainstrength,
                                   num_reads=numruns,
                                   label=label)

    return response


def show_inspector(response):
    dwave.inspector.show(response)
