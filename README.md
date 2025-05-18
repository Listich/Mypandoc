---

# 📝 mypandoc

Bienvenue dans **mypandoc**, un projet Haskell inspiré de [Pandoc](https://pandoc.org/) — le célèbre convertisseur de documents.  
🎯 L’objectif est de parser un document d’un certain format (XML/JSON/Markdown) et de le convertir dans un autre.

---

## 🚀 Fonctionnalités

- ✅ Lecture de fichiers au format **XML** et **JSON** *(Markdown en bonus)*
- 🔁 Conversion entre **XML**, **JSON** et **Markdown**
- 🛠️ Implémentation d’une **librairie de parsing maison** en Haskell
- ⚙️ Gestion de la ligne de commande avec options personnalisées
- 🧪 Prêt pour les tests automatiques et la soutenance

---

## 🧠 Architecture du projet

```
mypandoc/
├── src/
│   ├── Main.hs                -- point d’entrée du programme
│   ├── Document.hs            -- structure de document (title, body, etc.)
│   ├── Usage.hs               -- affichage du message d’usage
│   ├── XmlParser.hs           -- parseur XML vers Document

├── Makefile                   -- build avec stack
├── stack.yaml                 -- configuration stack
└── package.yaml               -- description du package
```

---

## 💻 Utilisation

### 📦 Compilation

```bash
make
```

> Ce `Makefile` s’occupe de builder le projet via `stack build`.

---

### 📂 Exécution

```bash
./mypandoc -i fichier_entree -f format_sortie [-o fichier_sortie] [-e format_entree]
```

#### 📌 Options :

| Option | Description |
|--------|-------------|
| `-i`   | chemin vers le fichier d’entrée (**obligatoire**) |
| `-f`   | format de sortie (**xml**, **json**, **markdown**) (**obligatoire**) |
| `-o`   | chemin du fichier de sortie *(si absent, affiche dans le terminal)* |
| `-e`   | format du fichier d’entrée *(si absent, tentative de détection automatique)* |

#### 🧪 Exemple :

```bash
./mypandoc -i examples/example.xml -f markdown
```

---

## 🛡️ Gestion des erreurs

- ❌ En cas d’erreur de parsing, d’argument manquant ou invalide → le programme retourne un **code 84**
- ✅ Si tout se passe bien → le code retour est **0**

---

## 🧠 À propos du parsing

Le projet repose sur une **librairie de parsing faite maison** en Haskell.  
Aucune bibliothèque externe de parsing n’est autorisée (genre Parsec, Megaparsec, etc.).

> Le parsing transforme le contenu brut en une structure `Document` définie dans `Document.hs`.

---

## ✨ Bonus possibles

- 📥 Lecture du Markdown en entrée
- 🧪 Plus de tests unitaires
- 📄 Documentation plus complète
- 📎 Ajout de nouveaux formats
- 🧙 Syntaxe avancée (images, liens, mise en forme...)

---

## 🧑‍💻 Équipe

- 👩‍💻 **serena.kifoula@epitech.eu** — parsing, structure des documents, gestion des formats d’entrée  
- 👨‍💻 **sven.reichert@epitech.eu** — conversion de documents, logique de CLI, formats de sortie

---
