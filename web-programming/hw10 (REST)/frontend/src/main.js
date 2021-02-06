import Vue from 'vue'
import App from './App.vue'

Vue.config.productionTip = false;

import data from './data';

var vm = new Vue({
    data: function() {
        return data;
    },
    render: h => h(App)
}).$mount('#app');

global.vm = vm;
