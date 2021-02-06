/**
 * Возвращает новый emitter
 * @returns {Object}
 */
function getEmitter() {
    let state = new Map()

    return {

        /**
         * Подписаться на событие
         * @param {String} event
         * @param {Object} context
         * @param {Function} handler
         */
        on: function (event, context, handler, several = null, through = null) {
            if (!state.has(event)) {
                state.set(event, [])
            }

            state.get(event).push([context, handler, 0, several, through, 0])

            return this
        },

        /**
         * Отписаться от события
         * @param {String} event
         * @param {Object} context
         */
        off: function (event, context) {
            let newState = new Map();

            [...state.keys()].forEach(k => {
                const ctxNhandlers = state.get(k)

                if (k === event || k.startsWith(event + '.')) {
                    newState.set(
                        k,
                        ctxNhandlers.filter(cNx => cNx[0] !== context))
                } else {
                    newState.set(k, state.get(k))
                }

            })


            state.clear();
            [...newState.keys()].forEach(k => {
                state.set(k, newState.get(k))
            })

            return this
        },

        /**
         * Уведомить о событии
         * @param {String} event
         */
        emit: function (event) {
            const suffixes = getSuffixes(event).reverse()

            suffixes.filter(k => state.has(k))
                .forEach(key => {
                    const ctxNhandlers = state.get(key)

                    ctxNhandlers.forEach(ctxNhandler => {
                        const ctx = ctxNhandler[0]
                        const handler = ctxNhandler[1]

                        ctxNhandler[2]++

                        if (ctxNhandler[3] === null && ctxNhandler[4] === null) {
                            handler.apply(ctx)
                        } else if (ctxNhandler[3] !== null) {
                            if (ctxNhandler[2] <= ctxNhandler[3]) {
                                handler.apply(ctx)
                            }
                        } else if (ctxNhandler[4] !== null) {
                            if (ctxNhandler[5] % ctxNhandler[4] === 0) {
                                handler.apply(ctx)
                            }
                        }

                        ctxNhandler[5] = (ctxNhandler[5] + 1) % ctxNhandler[4]
                    })
                })

            return this
        },

        /**
         * Подписаться на событие с ограничением по количеству полученных уведомлений
         * @star
         * @param {String} event
         * @param {Object} context
         * @param {Function} handler
         * @param {Number} times – сколько раз получить уведомление
         */
        several: function (event, context, handler, times) {
            if (times <= 0) {
                return this.on(event, context, handler)
            }

            return this.on(event, context, handler, times, null)
        },

        /**
         * Подписаться на событие с ограничением по частоте получения уведомлений
         * @star
         * @param {String} event
         * @param {Object} context
         * @param {Function} handler
         * @param {Number} frequency – как часто уведомлять
         */
        through: function (event, context, handler, frequency) {
            if (frequency <= 1) {
                return this.on(event, context, handler)
            }
            return this.on(event, context, handler, null, frequency)
        }
    };
}

function getSuffixes(s) {
    if (typeof s !== "string") {
        throw new TypeError('String expected.')
    }

    let suffxs = []
    let splitted = s.split('.')

    for (let i = 0; i < splitted.length; i++) {
        suffxs.push(splitted.slice(0, i + 1).join('.'))
    }

    return suffxs
}


module.exports = {
    getEmitter
};

