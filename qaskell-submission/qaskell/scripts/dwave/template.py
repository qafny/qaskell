###
# NOTE: This script requires the D-Wave credentials in the form of an API key to run.
###

from dwave.system import DWaveSampler, EmbeddingComposite

chainstrength = 1
numruns = 1000


def main():
    h = {}
    J = {}

# INSERT_HERE

    sampler = EmbeddingComposite(DWaveSampler())
    sampleset = sampler.sample_ising(h, J, chain_strength=chainstrength, num_reads=numruns, label="D-Wave Example Ising")
    print(sampleset)


if __name__ == '__main__':
    main()
