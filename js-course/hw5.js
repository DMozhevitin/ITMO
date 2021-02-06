'use strict';

/**
 *
 * @param {Object[]} friends
 * @return queue of friends for iterator
 */
function mkQueue(friends) {
    const used = new Set()
    const name2friend = new Map()
    const layers = []

    friends.forEach(f => name2friend.set(f.name, f))

    const q = []
    friends.filter(f => f.best).forEach(f => {
        f.level = 0
        used.add(f.name)
    })
    q.push(...friends.filter(f => f.best))


    while (q.length > 0) {
        const v = q[0]
        q.shift()

        if (!layers[v.level]) {
            layers[v.level] = []
        }

        layers[v.level].push(v)

        for (let i = 0; i < v.friends.length; i++) {
            const to = name2friend.get(v.friends[i])

            if (!to) {
                continue
            }

            if (!used.has(to.name)) {
                to.level = v.level + 1
                q.push(to)
                used.add(to.name)
            }
        }
    }

    const cmp = (f1, f2) => {
        if (f1.name < f2.name) {
            return -1
        }

        if (f1.name > f2.name) {
            return 1
        }

        return 0
    }

    layers.forEach(l => l.sort(cmp))

    const queue = []
    layers.forEach(l => queue.push(...l))

    return queue
}

/**
 * Итератор по друзьям
 * @constructor
 * @param {Object[]} friends
 * @param {Filter} filter
 */
function Iterator(friends, filter, maxLevel= Infinity) {
    if (!(filter instanceof Filter)) {
        throw new TypeError('Filter expected')
    }

    const queue = mkQueue(friends).filter(f => filter.filter(f) && f.level < maxLevel)
    let cur = 0

    this.next = function() {
        if (this.done()) {
            return null
        }

        return queue[cur++]
    }

    this.done = function() {
        return cur >= queue.length
    }
}

/**
 * Итератор по друзям с ограничением по кругу
 * @extends Iterator
 * @constructor
 * @param {Object[]} friends
 * @param {Filter} filter
 * @param {Number} maxLevel – максимальный круг друзей
 */
function LimitedIterator(friends, filter, maxLevel) {
    if (!(filter instanceof Filter)) {
        throw new TypeError('Filter expected')
    }

    Iterator.call(this, friends, filter, maxLevel)
}

LimitedIterator.prototype = Object.create(Iterator.prototype)
LimitedIterator.prototype.constructor = LimitedIterator

/**
 * Фильтр друзей
 * @constructor
 */
function Filter() {
    this.filter = _ => true
}

/**
 * Фильтр друзей
 * @extends Filter
 * @constructor
 */
function MaleFilter() {
    this.filter = f => f.gender === 'male'
}

MaleFilter.prototype = Object.create(Filter.prototype)
MaleFilter.prototype.constructor = MaleFilter

/**
 * Фильтр друзей-девушек
 * @extends Filter
 * @constructor
 */
function FemaleFilter() {
    this.filter = f => f.gender === 'female'
}

FemaleFilter.prototype = Object.create(Filter.prototype)
FemaleFilter.prototype.constructor = FemaleFilter

exports.Iterator = Iterator;
exports.LimitedIterator = LimitedIterator;

exports.Filter = Filter;
exports.MaleFilter = MaleFilter;
exports.FemaleFilter = FemaleFilter;

