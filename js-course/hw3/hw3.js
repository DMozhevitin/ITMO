'use strict';


/**
 * @param {Object} schedule Расписание Банды
 * @param {number} duration Время на ограбление в минутах
 * @param {Object} workingHours Время работы банка
 * @param {string} workingHours.from Время открытия, например, "10:00+5"
 * @param {string} workingHours.to Время закрытия, например, "18:00+5"
 * @returns {Object}
 */
function getAppropriateMoment(schedule, duration, workingHours) {
    let starts = []
    let ends = []

    let firstSuitableTime = {
        start: 0,
        end: 0
    }

    let exists;

    const tl = createTimeline(schedule, transformBankSchedule(workingHours))

    let state = {
        bank: false
    }

    let names = new Set()
    Object.keys(schedule).forEach(n => names.add(n))
    names.forEach(n => state[n] = true)

    let start = -1

    for (let point of tl) {
        const wasFree = isAllFree(state)

        if (point.bank) {
            state['bank'] = point.open
        } else {
            state[point.name] = !point.open
        }

        if (!isAllFree(state) && wasFree && point.x - start >= duration) {
            starts.push(start)
            ends.push(point.x)
        } else if (isAllFree(state) && !wasFree) {
            start = point.x
        }
    }

    if (starts.length > 0) {
        firstSuitableTime.start = starts[0]
        firstSuitableTime.end = ends[0]
    }

    if (starts.length > 0) {
        exists = true
    }

    const noanswer = starts.length === 0

    return {
        /**
         * Найдено ли время
         * @returns {boolean}
         */
        exists() {
            return !noanswer;
        },

        /**
         * Возвращает отформатированную строку с часами
         * для ограбления во временной зоне банка
         *
         * @param {string} template
         * @returns {string}
         *
         * @example
         * ```js
         * getAppropriateMoment(...).format('Начинаем в %HH:%MM (%DD)') // => Начинаем в 14:59 (СР)
         * ```
         */
        format(template) {
            const bankTz = bankTimezone(workingHours)

            if (noanswer) {
                return ''
            }

            let t = firstSuitableTime.start + bankTz * HOUR

            if (!number2Day.has(Math.floor(t / DAY))) {
                throw new Error('Invalid state')
            }

            const dd = number2Day.get(Math.floor(t / DAY))
            t %= DAY

            let hh = Math.floor(t / 60)
            if (hh < 10) {
                hh = '0' + hh
            }

            let mm = t % 60
            if (mm < 10) {
                mm = '0' + mm
            }

            return template.replace('%DD', dd)
                .replace('%HH', '' + hh)
                .replace('%MM', '' + mm)
        },

        /**
         * Попробовать найти часы для ограбления позже [*]
         * @note Не забудь при реализации выставить флаг `isExtraTaskSolved`
         * @returns {boolean}
         */
        tryLater() {
            if (!exists) {
                return false;
            }

            if (firstSuitableTime.start + duration + 30 > firstSuitableTime.end) {
                const suitableTimes = zip(starts, ends).filter(t => firstSuitableTime.start + 30 <= t[0])
                if (suitableTimes.length === 0) {
                    exists = false
                } else {
                    firstSuitableTime.start = suitableTimes[0][0]
                    firstSuitableTime.end = suitableTimes[0][1]
                }
            } else {
                firstSuitableTime.start += 30
            }

            return exists
        }
    };
}

function zip(a, b) {
    if (!(Array.isArray(a) && Array.isArray(b))) {
        throw new TypeError('Arrays expected.')
    }

    if (a.length !== b.length) {
        throw new Error('Arrays with same length expected.')
    }

    let ans = []

    for (let i = 0; i < a.length; i++) {
        ans.push([
            a[i],
            b[i]
        ])
    }

    return ans
}

function isAllFree(state) {
    return Object.values(state).every(_ => _)
}

const HOUR = 60
const DAY = 24 * HOUR

const day2MinsAdd = new Map(
    [
        ['ПН', 0],
        ['ВТ', DAY],
        ['СР', DAY * 2]
    ]
)

const number2Day = new Map(
    [
        [0, 'ПН'],
        [1, 'ВТ'],
        [2, 'СР'],
    ]
)

function bankTimezone(bankSchedule) {
    return +(bankSchedule.from.split('+')[1])
}

function parseTime(t) {
    let splitted = t.split('+')
    const time = splitted[0]
    const zone = splitted[1]

    splitted = time.split(':')

    return +splitted[0] * HOUR + +splitted[1] - +zone * HOUR
}

function parseScheduleString(str) {
    if (typeof str !== 'string') {
        throw new TypeError('String expected.')
    }

    let splitted = str.split(" ")

    if (splitted.length !== 2) {
        throw new Error('Invalid argument')
    }

    const day = splitted[0]
    const timeWithZone = splitted[1]

    if (!day2MinsAdd.has(day)) {
        throw new Error('Invalid argument: ' + day)
    }


    return parseTime(timeWithZone) + day2MinsAdd.get(day)
}

function createFromToObj(d, schedule) {
    return {
        from: d + ' ' + schedule.from,
        to: d + ' ' + schedule.to
    }
}

function transformBankSchedule(schedule) {
    return {
        bank: [...day2MinsAdd.keys()].map(d => createFromToObj(d, schedule))
    }
}

function createTimeline(gangSchedule, bankSchedule) {
    if (typeof gangSchedule !== 'object') {
        throw new TypeError('Object expected.')
    }

    const timeline = []

    for (let fr of Object.keys(gangSchedule)) {
        gangSchedule[fr].forEach(o => {
            timeline.push({
                open: true,
                name: fr,
                x: parseScheduleString(o.from)
            })

            timeline.push({
                open: false,
                name: fr,
                x: parseScheduleString(o.to)
            })
        })
    }

    bankSchedule.bank.forEach(o => {
        timeline.push({
            open: true,
            bank: true,
            x: parseScheduleString(o.from)
        })

        timeline.push({
            open: false,
            bank: true,
            x: parseScheduleString(o.to)
        })
    })

    timeline.sort((a, b) => {
        if (a.x === b.x) {
            if (!a.open && b.open) {
                return -1;
            }
        } else {
            return a.x - b.x;
        }
    })

    return timeline
}

const isExtraTaskSolved = true

module.exports = {
    getAppropriateMoment,
    isExtraTaskSolved
};
