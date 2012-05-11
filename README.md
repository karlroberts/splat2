NAME
        splat2 - helps manage configuration to improve quality assurance.

SYNOPSIS
        splat [OPTIONS] [-t TEMPLATE-FILE:TEMPLATE-FILE...] [-d TEMPLATE-DIR:TEMPLATE-DIR:...] {ENV-PATHS}
        splat -p {PROPERTIES} [--pretty] {ENV-PATHS}


DESCRIPTION
        mode 1
            splat [OPTIONS] [-t TEMPLATE-FILE:TEMPLATE-FILE...] [-d TEMPLATE-DIR:TEMPLATE-DIR:...] [-o OUTPUTDIR] {ENV-PATHS}
                generates files from the templates found populated with the properties found.
                If no ENV-PATH is supplied it is assumed that all are wanted


PHILOSOPHY
        Splat2 is a tool to help manage configuration of software across all environments in the SDLC from dev all the
        way to production under tight version control.

        Configuration is code! It therefore must be treated with the same version control rigour as your source code.
        Enterprises especially find this difficult, having different teams manage different environments and controlling
        the hardware in each environment. Also each environment may have the same or similar logical layout but be
        radically different physically, eg local-dev may use a fast in memory database while production uses a remote
        Oracle instance.

        Configuration files in each environment are usually used to allow for these differences. The problem is that if
        these files are managed by different teams, they can get out of sync and cause a problem at deploy/run-time.
        For example developers may extract a new property from a previously hard coded value eg a URL and add it to
        local config. However this property needs to be propagated to all environments on the way to production and
        also contain the correct value if it is different in each environment. That propagation needs to be
        synchronised with the software deployment to that environment, because if it happens too soon, it may break the
        version of software already in that environment. Clearly this is a version control problem.

        A key principle is that all artifacts in production including and especially configuration should be able to
        be managed in source control and known 'a priori' at software build time. To enable smooth continuous delivery
        each environment should have "exactly" the same deploy process scripted. Splat2 enables this by allowing all the
        properties that change between environments to be held in a tree-like structure in a file under version control
        with the source code. Additionally, template files with placeholders for the appropriate properties are also kept
        under version control. "splat" or its derived  Ant macro, maven plugin or sbt plugin can then be used at
        build-time to splatter the properties for the various environments into the templates that expect them ....

OPTIONS
    Property Options
        -p {PROPERTIES}
            prints out the name value pairs of properties found under a given path
            PROPERTIES is an optional colon(:) separated list of property names.
            A special property pattern '*" means "all properties". if used it should be used alone, eg
            splat -p *
            The output from this command are name value pairs describing the property

        -pp, --pretty
            pretty print. By default the splat command is meant to be used in scripts and so it's output





STATE
        The `splat` code is in pre-alpha dev mode. It is not suitable for use... yet



AUTHOR
        Karl Roberts <karl.roberts@owtelse.com>

NOTES
        1. official repository
           https://github.com/karlroberts/splat2
        2. site and documentation
           http://??
        3. license (3 point BSD style)
           https://github.com/karlroberts/splat2/blob/master/LICENSE