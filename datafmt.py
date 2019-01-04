#!/usr/bin/env python3

import sqlite3

def main():
    results = get_results()
    grouped_results = {}
    for r in results:
        try:
            grouped_results[r[0]].append((r[1], r[2]))
        except KeyError:
            grouped_results[r[0]] = [(r[1], r[2])]
    for trip in grouped_results.keys():
        progress_to_show = list(filter(lambda x: x[1] != None, grouped_results[trip]))
        if len(progress_to_show) > 0:
            print("trip_" + str(trip))
            for progress in progress_to_show:
                print("{:f} \"{:s}\"".format(progress[1], progress[0]))
            print("")
            print("")

def get_results():
    conn = sqlite3.connect('locations.db')
    cur = conn.cursor()
    cur.execute("SELECT trip_id, timestamp, progress FROM location")
    return cur.fetchall()

main()
