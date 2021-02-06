<template>
    <div class="middle">
        <Sidebar :posts="posts" :users="users"/>
        <main>
            <Index :users="users" :posts="posts" :comments="comments" v-if="page === 'Index'"/>
            <Enter v-if="page === 'Enter'"/>
            <Register v-if="page === 'Register'"/>
            <AddPost v-if="page === 'AddPost'"/>
            <EditPost v-if="page === 'EditPost'"/>
            <Users :users="users" v-if="page === 'Users'"></Users>
            <FullPost :comments="comments" :users="users" :post="posts[id]" v-if="page.startsWith('Post/')"></FullPost>
        </main>
    </div>
</template>
<script>
    import Index from './middle/Index';
    import Enter from './middle/Enter';
    import Register from './middle/Register';
    import AddPost from './middle/AddPost';
    import EditPost from "./middle/EditPost";
    import Users from "./middle/Users";
    import FullPost from "./middle/FullPost";
    import Sidebar from "./Sidebar";

    export default {
        name: "Middle",
        props: ['users', 'posts', 'comments'],
        data: function () {
            return {
                page: "Index",
                id: null
            }
        },
        components: {
            FullPost,
            Users,
            EditPost,
            Index,
            Enter,
            Register,
            AddPost,
            Sidebar
        }, beforeCreate() {
            this.$root.$on("onChangePage", (page, id) => {
                this.page = page;
                this.id = id;
            });
        }
    }
</script>

<style scoped>
</style>
