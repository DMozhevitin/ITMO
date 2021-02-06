'use strict';

const weather = require('./weather');

/**
 * Как выбрать geoid для тестирования функции:
 * Заходим на https://yandex.ru/pogoda, в поиске вводим желаемый город
 * Получаем урл вида https://yandex.ru/pogoda/10451 - 10451 это geoid
 */
const geoids = [2, 5, 7, 10, 11, 14, 213];

async function main() {
  let res = await weather
    .planTrip(geoids)
    .cloudy(1)
    .sunny(1)
    .max(0)
    .build()
  console.log(res)
}

main().catch(console.error);
