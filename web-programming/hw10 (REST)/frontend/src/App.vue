<template>
    <!--suppress HtmlUnknownTag -->
    <body id="app">
    <Header :user="user"/>
    <Middle :user="user" :posts="posts"/>
    <Footer/>
    </body>
</template>

<script>
    import Header from './components/Header'
    import Middle from './components/Middle'
    import Footer from './components/Footer'
    import axios from 'axios'

    export default {
        name: 'app',
        data: function () {
            return {
                user: null,
                posts: []
            }
        },
        components: {
            Header,
            Middle,
            Footer
        }, beforeCreate() {
            axios.get("/api/1/posts").then(posts => this.posts = posts["data"]);

            this.$root.$on("onLogout", () => {
                localStorage.removeItem("jwt");
                this.user = null;
            });

            this.$root.$on("onRegister", (login, password, name) => {
                axios.post('api/1/users', {
                    login: login,
                    password: password,
                    name: name
                }).then(response => {
                    if (response.data) {
                        this.$root.$emit("onEnter", login, password)
                    }
                }).catch(error => {
                    this.$root.$emit("onRegisterValidationError", error.response.data)
                })
            });

            this.$root.$on("onJwt", (jwt, enter) => {
                axios.defaults.headers = {
                    Authorization: "Bearer " + jwt
                };

                axios.get("/api/1/users/authorized").then(response => {
                    this.user = response.data;
                    if (enter) {
                        this.$root.$emit("onEnterSuccess");
                    }
                });
            });

            this.$root.$on("onAddPost", (title, text) => {
               axios.post("/api/1/posts", {
                   title: title,
                   text: text
               })
                   .then(
                       axios.get("/api/1/posts")
                           .then(response => {
                               // alert(123);
                               this.posts = response["data"]
                           })
                   )
                   .then(this.$root.$emit("onChangePage", "Index"))
                   .catch(error => {
                        this.$root.$emit("onAddPostValidationError", error.response.data);
                   })
            });

            this.$root.$on("onEnter", (login, password) => {
                axios.post("/api/1/jwt", {
                    login: login,
                    password: password
                }).then(response => {
                    localStorage.setItem("jwt", response.data);
                    this.$root.$emit("onJwt", response.data, true);
                }).catch(error => {
                    this.$root.$emit("onEnterValidationError", error.response.data);
                });
            });
        }, beforeMount() {
            if (localStorage.getItem("jwt") && !this.user) {
                this.$root.$emit("onJwt", localStorage.getItem("jwt"), true);
            }
        }
    }
</script>

<style>
</style>
