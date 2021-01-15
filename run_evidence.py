import tempfile
import os

smoker_drinkers_str1 = '''
person = {0, 1, 2, 3, 4, 5}
//, Wannes, Jesse, Luc, 1, 2, 3, 4, 5, 6}
friends(person,person)
smokes(person)
stress(person)
drinks(person)
1.22 stress(x) => smokes(x)
2.08 friends(x,y) ^ smokes(x) => smokes(y)
2 stress(x) => drinks(x)
1.5 friends(x,y) ^ drinks(x) => drinks(y)
'''

smoker_drinkers_str2 = '''
person = {0, 1, 2, 3, 4, 5}
//, Wannes, Jesse, Luc, 1, 2, 3, 4, 5, 6}
friends(person,person)
smokes(person)
drinks(person)
1.22 smokes(x)
2.08 friends(x,y) ^ smokes(x) => smokes(y)
0.69 friends(x,y)
1.5 drinks(x) ^ friends(x,y) => drinks(y)
'''

friends_smokers_str = '''
person = {0, 1, 2, 3, 4, 5}
//, Wannes, Jesse, Luc, 1, 2, 3, 4, 5, 6}
friends(person,person)
smokes(person)
1.22 smokes(x)
2.08 friends(x,y) ^ smokes(x) => smokes(y)
0.69 friends(x,y)
'''

evidences = [
    'friends(0,2).',
    'friends(1,4).',
    '!friends(2,4).',
    '!friends(4,3).',
    'friends(4,5).',
    '!friends(4,2).',
    'friends(0,1).',
    'friends(1,3).',
    '!friends(3,4).',
    '!friends(0,3).',
    'friends(2,5).',
    'friends(0,4).'
]

for i in range(len(evidences)):
    with tempfile.NamedTemporaryFile(mode='w', delete=False) as tf:
        tf.write(smoker_drinkers_str2)
        for j in range(i + 1):
            tf.write(evidences[j] + '\n')
        tf.flush()
        os.system(
            'java -jar target/scala-2.11/forclift.jar -z --format-in mln {} > logs/fr_sm_dr2_new/{}.log'.format(
                tf.name, i + 1
            )
        )
