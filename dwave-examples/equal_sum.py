from collections import defaultdict
import helpers


print_embedding = True

A = 1
B = 1

num_list = [1, 3, 2]


def main():
    Q = defaultdict(int)

    S = sum(num_list)
    for i in range(len(num_list)):
        Q[(i, i)] = A * num_list[i] * (num_list[i] - S)

        for j in range(i + 1, len(num_list)):
            Q[(i, j)] = B * num_list[i] * num_list[j]

    if print_embedding:
        embedding = helpers.get_minor_embedding(Q)
        print(embedding)

    response = helpers.run_on_dwave(Q, label="Equal Sum", chainstrength=0, numruns=100)
    response_data = response.data(fields=['sample', 'energy', 'num_occurrences'])

    print('-' * 130)
    print('{:>70s}{:>70s}{:>30s}{:^30s}'.format('List 1', 'List 2', 'Energy', 'Num Occurrences'))
    print('-' * 130)
    for sample, E, num_occurrences in response_data:
        nums_1, nums_2 = [], []
        for key, is_lst_1 in sample.items():
            idx = key - 1
            if is_lst_1:
                nums_1.append(num_list[idx])
            else:
                nums_2.append(num_list[idx])

        print('{:>70s}{:>70s}{:>30s}{:^30s}'.format(nums_1, nums_2, str(E), num_occurrences))

    helpers.show_inspector(response)


if __name__ == '__main__':
    main()
