<template>
    <section>
        <div class="header">
            By {{post.user.name}}
        </div>
        <div class="body">
            {{post.text}}
        </div>
        <div class="footer">
            <a :href="'#Post/' + post.id" @click="changePage('Post/' + post.id, post.id)">View all</a>
        </div>
    </section>

</template>

<script>
    export default {
        props: ['post'],
        name: "SidebarPost",
        methods: {
            changePage: function(page, id) {
                this.$root.$emit("onChangePage", page, id)
            }
        }
    }
</script>

<style scoped>

</style>
