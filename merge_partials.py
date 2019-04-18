import json, sys

def merge_dicts(a, b):
    all_keys = set(a.keys()) | set(b.keys())
    new = dict()
    for k in all_keys:
        if k in a and k not in b:
            new[k] = a[k]
        elif k in b and k not in a:
            new[k] = b[k]
        else:
            new[k] = a[k] + b[k]
    return new

def merge_dicts(a, b):
    all_keys = set(a.keys()) | set(b.keys())
    new = dict()
    for k in all_keys:
        if k in a and k not in b:
            new[k] = a[k]
        elif k in b and k not in a:
            new[k] = b[k]
        else:
            new[k] = a[k] + b[k]
    return new


def merge_stats(a, b):
    new_stats = dict()
    for k in a['stats'].keys():
        new_stats[k] = merge_dicts(a['stats'][k], b['stats'][k])
    return new_stats

def merge_data(a, b):
    new = dict()
    new['stats'] = merge_stats(a, b)
    new['size'] = a['size'] + b['size']
    new['numSuccesses'] = a['numSuccesses'] + b['numSuccesses']
    new['numFailures'] = a['numFailures'] + b['numFailures']
    new['data'] = a['data'] + b['data']
    return new

def merge_all_data(*items):
    current = items[0]
    for i in items[1:]:
        current = merge_data(current, i)
    return current

if __name__ == '__main__':
    paths = sys.argv[1:]
    if len(paths) > 0:
        items = list()
        for p in paths:
            with open(p) as f:
                items.append(json.load(f))
        merged = merge_all_data(*items)
        with open('merged.json', 'w') as f:
            json.dump(merged, f)
