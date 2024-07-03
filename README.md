<a name="top-of-readme"></a>
<br />
<div align="center">
<img src="https://github.com/eldr-io/hastl/assets/83576392/0da858b9-7f42-4be9-aa6f-336332884400" width="50%" />

<h3 align="center">Production ready, modern web-application starter template</h3>

<p align="center"> hastl is a modern Haskell web application using <b>(H)</b>tmx, <b>(A)</b>lpine.js, <b>(S)</b>ervant, <b>(T)</b>ailwind-css and <b>(L)</b>ucid. It is based on the awesome <a href="https://github.com/parsonsmatt/servant-persistent">servant-persistent</a> example and is licensed under <a href="https://github.com/eldr-io/hastl/blob/main/LICENSE.md">MIT</a> and is entirely free and open source.
</p>

#### Built with

[![Haskell][Haskell]][Haskell-url]
[![Htmx][Htmx]][Htmx-url]
[![Alpine][Alpine]][Alpine-url]
[![Servant][Servant]][Servant-url]
[![Tailwind][Tailwind]][Tailwind-url]
[![Lucid][Lucid]][Lucid-url]

hastl is primarily tested with PostgreSQL but it uses the popular <a href="https://www.yesodweb.com/book/persistent">persistent</a> haskell library as an ORM and therefore can be used with most popular databases.

### <img height="20" width="20" src="https://cdn.jsdelivr.net/npm/simple-icons@v12/icons/rocket.svg" style="margin-right: 0.5rem" />  Features 

<p align="left"><b>Type-safe APIs</b> - hastl uses Servant to define APIs that make up the application, providing type safety and consistency across the application, with the haskell type-checker keeping you honest as you develop
</p>
<p align="left"><b>Modern frontend with htmx</b> - the use of htmx in hastl allows you to build modern user interfaces with the simplicity and power of hypertext, with haskell and lucid2 doing the heavy lifting
</p>
<p align="left"><b>Live reloading with GHCID</b> - since we're writing almost all of our code in the backend, hastl uses GHCID to instantly reload your project as you make changes
</p>
<p align="left"><b>Integration testing with TestContainers</b> - hastl uses <a href="https://testcontainers.com">testcontainers</a> to spin up a database on the fly to run integration tests and give you confidence in your business logic
</p>


### Get Started
</div>

Start by creating a new repo from the hastl template by clicking "Use this template" in the top right corner.

#### Setting up dependencies

Hastl requires the tailwindcss standalone CLI executable locally in the root folder as <a href="https://tailwindcss.com/blog/standalone-cli">described here</a>. In addition, it expects a local postgreSQL database to be running on port 5432, alternatively use the provided compose.yaml file to run either Podman or Docker to spin it up.

The project includes a Makefile to provide you with convenient targets for running the server in development mode as well as running the tailwindcss CLI in watch mode to generate stylesheets on the fly.

Navigate into your cloned repo and run:

```
make run
```
This will build and run hastl and you should be able to navigate to `localhost:8081` in your browser and see the hastl demo application:

![Screenshot from 2024-06-08 15-41-54](https://github.com/eldr-io/hastl/assets/83576392/19af0d8e-33b8-411e-a19e-e2e4f8c3420f)

To run the development live reloading mode, make sure that <a href="https://github.com/ndmitchell/ghcid">ghcid</a> is installed and then use the ghcid-devel target:

```
make ghcid-devel
```

#### Running unit tests

The unit tests of the project can be found in the `test` directory and can assert things like HTML generation from database types. 

You can run all the unit tests with:

```
make test
```

#### Running integration tests

Hastl ships with built-in integration tests that use testcontainers to start a local postgreSQL database inside a container (using docker or podman) on-the-fly, as well as running the project web-server, allowing the tests to exercise the actual HTTP endpoints to assert correctness.

The integration tests manage the containers, starting them and tearing them down as the tests complete.

You can run all the integration tests with:
```
make test-integration
```

#### Changing the routes and templates

Hastl allows you to combine strongly-typed Servant APIs to make up your application. To add a new route and endpoint, you can create a new file similar to `lib/Api/User.hs` e.g. if you wanted to create a Todo-list API you could create `lib/Api/Todo.hs`. Additionally, you can create a new directory within `lib/Api/Templates` to store your Lucid-powered Haskell template files. Within the template files, you have access to the full power of HTMX and Alpine through helper functions.

If you wish to use persistent models in your application, you can define your models in `lib/Models.hs` and persistent will automatically create the Haskell types, as well as handling the database migrations for DEVELOPMENT setups (note: is it recommended to use a more robust migration mechanism for production).

[Haskell]: https://img.shields.io/badge/haskell-5D4F85?style=for-the-badge&logo=haskell&logoColor=white
[Haskell-url]: https://haskell.org
[Htmx]:  https://img.shields.io/badge/htmxjs-3366CC?style=for-the-badge&logo=htmx&logoColor=white
[Htmx-url]: https://htmx.org
[Alpine]: https://img.shields.io/badge/alpinejs-8BC0D0?style=for-the-badge&logo=alpine.js&logoColor=white
[Alpine-url]: https://alpinejs.dev
[Servant]: https://img.shields.io/badge/Servant-5D4F85?style=for-the-badge&logo=haskell&logoColor=white"
[Servant-url]: https://www.servant.dev
[Tailwind]: https://img.shields.io/badge/Tailwind-06B6D4?style=for-the-badge&logo=tailwindcss&logoColor=white
[Tailwind-url]: https://tailwindcss.com
[Lucid]: https://img.shields.io/badge/Lucid-5D4F85?style=for-the-badge&logo=haskell&logoColor=white
[Lucid-url]: https://hackage.haskell.org/package/lucid2

