<template>
    <div>
        <MiddlePost v-for="post in posts" :post="post" :key="post.id"/>
    </div>
</template>

<script>
    import axios from 'axios'
    import MiddlePost from './MiddlePost'

    export default {
        name: "Index",
        props: ['posts'],
        beforeCreate() {
            axios.get('api/1/posts').then(response => this.posts = response.data)
        },
        components: {
            MiddlePost
        }
    }
</script>

<style scoped>

</style>
